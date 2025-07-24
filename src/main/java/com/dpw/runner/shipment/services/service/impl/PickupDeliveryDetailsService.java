package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.TiContainersRequest;
import com.dpw.runner.shipment.services.dto.request.TiLegsRequest;
import com.dpw.runner.shipment.services.dto.request.TiPackagesRequest;
import com.dpw.runner.shipment.services.dto.request.TiReferencesRequest;
import com.dpw.runner.shipment.services.dto.request.TiTruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.RAKCDetailsResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.InstructionType;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IKafkaAsyncService;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.TransportInstructionValidationUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class PickupDeliveryDetailsService implements IPickupDeliveryDetailsService {
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    private JsonHelper jsonHelper;

    private AuditLogService auditLogService;
    private IPartiesDao partiesDao;

    private CommonUtils commonUtils;
    private MasterDataUtils masterDataUtils;
    private ExecutorService executorService;

    private IKafkaAsyncService kafkaAsyncService;
    private TransportInstructionValidationUtil transportInstructionValidationUtil;
    private IContainerV3Service containerV3Service;
    private static final Pattern DIMENSION_PATTERN = Pattern.compile(
            "^\\s*(\\d+(\\.\\d+)?)\\s*[xX×]\\s*(\\d+(\\.\\d+)?)\\s*[xX×]\\s*(\\d+(\\.\\d+)?)\\s*$"
    );
    public static final Map<String, RunnerEntityMapping> tableNames = Map.ofEntries(
            Map.entry(Constants.TRANSPORT_DETAIL_SHIPMENT_ID, RunnerEntityMapping.builder().tableName(Constants.PICKUP_DELIVERY_DETAILS).dataType(Long.class).fieldName(Constants.TRANSPORT_DETAIL_SHIPMENT_ID).isContainsText(true).build()),
            Map.entry(Constants.TRANSPORT_DETAIL_ORG_CODE, RunnerEntityMapping.builder().tableName(Constants.TRANSPORT_DETAIL).dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build()),
            Map.entry(Constants.TRANSPORT_DETAIL_INSTRUCTION_TYPE, RunnerEntityMapping.builder().tableName(Constants.PICKUP_DELIVERY_DETAILS).dataType(InstructionType.class).fieldName(Constants.TRANSPORT_DETAIL_INSTRUCTION_TYPE).isContainsText(true).build()),
            Map.entry(Constants.TRANSPORT_DETAIL_TI_REFERENCE, RunnerEntityMapping.builder().tableName(Constants.PICKUP_DELIVERY_DETAILS).dataType(String.class).fieldName(Constants.TRANSPORT_DETAIL_TI_REFERENCE).isContainsText(true).build())
    );

    @Autowired
    public PickupDeliveryDetailsService(CommonUtils commonUtils, IPartiesDao partiesDao, IPickupDeliveryDetailsDao pickupDeliveryDetailsDao, JsonHelper jsonHelper, AuditLogService auditLogService, MasterDataUtils masterDataUtils, ExecutorService executorService,
                                        KafkaAsyncService kafkaAsyncService, TransportInstructionValidationUtil transportInstructionValidationUtil, IContainerV3Service containerV3Service) {
        this.pickupDeliveryDetailsDao = pickupDeliveryDetailsDao;
        this.jsonHelper = jsonHelper;
        this.auditLogService = auditLogService;
        this.partiesDao = partiesDao;
        this.commonUtils = commonUtils;
        this.masterDataUtils = masterDataUtils;
        this.executorService = executorService;
        this.kafkaAsyncService = kafkaAsyncService;
        this.transportInstructionValidationUtil = transportInstructionValidationUtil;
        this.containerV3Service = containerV3Service;
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return createPickupDeliveryDetails(commonRequestModel);
    }

    private ResponseEntity<IRunnerResponse> updateTransportInstruction(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request Id is null for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }
        if (StringUtility.isEmpty(request.getTiReferenceNumber())) {
            log.debug("TI Reference number is empty for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("TI Reference number is empty for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<PickupDeliveryDetails> oldEntity = pickupDeliveryDetailsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Pickup Delivery entity is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        transportInstructionValidationUtil.validateShipmentId(request);
        validateTransportInstructionLegs(request);
        validateTransportInstructionLegsTruckDriverDetails(request);
        validateTransportInstructionLegsReferenceNumbers(request);
        validateTransportInstructionLegsContainersDetails(request);
        validateTransportInstructionLegsPackagesDetails(request);
        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        pickupDeliveryDetails.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            beforeSave(pickupDeliveryDetails);
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        afterSave(pickupDeliveryDetails, false, request);

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    public ResponseEntity<IRunnerResponse> createTransportInstruction(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            String resp = "Request is empty for Pickup Delivery create with Request Id " + LoggerHelper.getRequestIdFromMDC();
            log.debug("Request is empty for Pickup Delivery create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(resp);
        }
        ShipmentDetails shipmentDetails = transportInstructionValidationUtil.validateShipmentId(request);
        validateTransportInstructionLegs(request);
        validateTransportInstructionLegsTruckDriverDetails(request);
        validateTransportInstructionLegsReferenceNumbers(request);
        validateTransportInstructionLegsContainersDetails(request);
        validateTransportInstructionLegsPackagesDetails(request);
        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        Long totalCount = pickupDeliveryDetailsDao.getTotalTransportInstructionCountIncludeDeleted(request.getShipmentId()) + 1;
        try {
            pickupDeliveryDetails.setTiReferenceNumber(shipmentDetails.getShipmentId() + "-" + String.format("%03d", totalCount));
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);
            afterSave(pickupDeliveryDetails, true, request);
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(null)
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("Pickup Delivery Details created successfully for Id {} with Request Id {}", pickupDeliveryDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    private void validateTransportInstructionLegsPackagesDetails(PickupDeliveryDetailsRequest request) {
        List<TiLegsRequest> tiLegsList = request.getTiLegsList();
        if (!CollectionUtils.isEmpty(tiLegsList)) {
            for (TiLegsRequest tiLegsRequest : tiLegsList) {
                List<TiPackagesRequest> packages = tiLegsRequest.getTiPackages();
                if (!CollectionUtils.isEmpty(packages)) {
                    for (TiPackagesRequest packagesRequest : packages) {
                        validateDimensions(packagesRequest.getDimensions());
                        validateGrossWeightForPackages(packagesRequest);
                        validateNetWeightForContainers(packagesRequest);
                        // Volume & Unit Validation
                        validateVolumeForContainers(packagesRequest);
                    }
                }
            }
        }
    }

    private static void validateGrossWeightForPackages(TiPackagesRequest packagesRequest) {
        if ((packagesRequest.getGrossWeight() != null && StringUtility.isEmpty(packagesRequest.getGrossWeightUnit())) ||
                (packagesRequest.getGrossWeight() == null && StringUtility.isNotEmpty(packagesRequest.getGrossWeightUnit()))) {
            throw new ValidationException("Packages: Gross weight and gross weight unit must both be provided or both be null.");
        }
        if (packagesRequest.getGrossWeight() != null && StringUtility.isNotEmpty(packagesRequest.getGrossWeightUnit())) {
            String combinedGrossWeight = packagesRequest.getGrossWeight().toPlainString() + packagesRequest.getGrossWeightUnit().trim();
            if (combinedGrossWeight.length() > 15) {
                throw new ValidationException("Packages: Combined length of grossWeight and grossWeightUnit must not exceed 15 characters.");
            }
        }
    }

    private static void validateVolumeForContainers(TiPackagesRequest packagesRequest) {
        if ((packagesRequest.getVolume() != null && StringUtility.isEmpty(packagesRequest.getVolumeUnit())) ||
                (packagesRequest.getVolume() == null && StringUtility.isNotEmpty(packagesRequest.getVolumeUnit()))) {
            throw new ValidationException("Packages: Volume and volume unit must both be provided or both be null.");
        }
        if (packagesRequest.getVolume() != null && StringUtility.isNotEmpty(packagesRequest.getVolumeUnit())) {
            String combined = packagesRequest.getVolume().toPlainString() + packagesRequest.getVolumeUnit().trim();
            if (combined.length() > 10) {
                throw new ValidationException("Packages: Combined length of volume and volumeUnit must not exceed 10 characters.");
            }
        }
    }

    private static void validateNetWeightForContainers(TiPackagesRequest packagesRequest) {
        // Net Weight & Unit Validation
        if ((packagesRequest.getNetWeight() != null && StringUtility.isEmpty(packagesRequest.getNetWeightUnit())) ||
                (packagesRequest.getNetWeight() == null && StringUtility.isNotEmpty(packagesRequest.getNetWeightUnit()))) {
            throw new ValidationException("Packages: Net weight and net weight unit must both be provided or both be null.");
        }
        if (packagesRequest.getNetWeight() != null && StringUtility.isNotEmpty(packagesRequest.getNetWeightUnit())) {
            String combined = packagesRequest.getNetWeight().toPlainString() + packagesRequest.getNetWeightUnit().trim();
            if (combined.length() > 15) {
                throw new ValidationException("Packages: Combined length of netWeight and netWeightUnit must not exceed 15 characters.");
            }
        }
    }

    public void validateDimensions(String dimensions) {
        if (StringUtility.isNotEmpty(dimensions)) {
            Matcher matcher = DIMENSION_PATTERN.matcher(dimensions);
            if (!matcher.matches()) {
                throw new ValidationException("Dimensions must be in the format LxWxH (e.g., 10x20x30).");
            }

            // Optional: parse values and validate further (e.g., non-zero, max size)
            double length = Double.parseDouble(matcher.group(1));
            double width = Double.parseDouble(matcher.group(3));
            double height = Double.parseDouble(matcher.group(5));

            if (length <= 0 || width <= 0 || height <= 0) {
                throw new ValidationException("Length, width, and height must be positive numbers.");
            }
        }
    }

    private void validateTransportInstructionLegsContainersDetails(PickupDeliveryDetailsRequest request) {
        List<TiLegsRequest> tiLegsList = request.getTiLegsList();
        if (!CollectionUtils.isEmpty(tiLegsList)) {
            for (TiLegsRequest tiLegsRequest : tiLegsList) {
                List<TiContainersRequest> containers = tiLegsRequest.getTiContainers();
                if (!CollectionUtils.isEmpty(containers)) {
                    Set<String> containerNumbers = new HashSet<>();
                    for (TiContainersRequest containersRequest : containers) {
                        validateContainerNumber(containerNumbers, containersRequest);
                        containerNumbers.add(containersRequest.getNumber());
                        validateGrossWeightForContainers(containersRequest);
                        // Net Weight & Unit Validation
                        validateNetWeightForContainers(containersRequest);
                        validateVolumeForContainers(containersRequest);
                    }
                }
            }
        }
    }

    private void validateContainerNumber(Set<String> containerNumbers, TiContainersRequest containersRequest) {
        if (containerNumbers.contains(containersRequest.getNumber())) {
            throw new ValidationException("Container Number cannot be same for two different containers in same legs");
        }
        ContainerNumberCheckResponse containerNumberCheckResponse = containerV3Service.validateContainerNumber(containersRequest.getNumber());
        if (containerNumberCheckResponse == null || !containerNumberCheckResponse.isSuccess()) {
            throw new ValidationException("Invalid container number");
        }
    }

    private static void validateGrossWeightForContainers(TiContainersRequest containersRequest) {
        if ((containersRequest.getGrossWeight() != null && StringUtility.isEmpty(containersRequest.getGrossWeightUnit())) ||
                (containersRequest.getGrossWeight() == null && StringUtility.isNotEmpty(containersRequest.getGrossWeightUnit()))) {
            throw new ValidationException("Containers: Gross weight and gross weight unit must both be provided or both be null.");
        }
        if (containersRequest.getGrossWeight() != null && StringUtility.isNotEmpty(containersRequest.getGrossWeightUnit())) {
            String combinedGrossWeight = containersRequest.getGrossWeight().toPlainString() + containersRequest.getGrossWeightUnit().trim();
            if (combinedGrossWeight.length() > 15) {
                throw new ValidationException("Containers: Combined length of grossWeight and grossWeightUnit must not exceed 15 characters.");
            }
        }
    }

    private static void validateVolumeForContainers(TiContainersRequest containersRequest) {
        // Volume & Unit Validation
        if ((containersRequest.getVolume() != null && StringUtility.isEmpty(containersRequest.getVolumeUnit())) ||
                (containersRequest.getVolume() == null && StringUtility.isNotEmpty(containersRequest.getVolumeUnit()))) {
            throw new ValidationException("Containers: Volume and volume unit must both be provided or both be null.");
        }
        if (containersRequest.getVolume() != null && StringUtility.isNotEmpty(containersRequest.getVolumeUnit())) {
            String combined = containersRequest.getVolume().toPlainString() + containersRequest.getVolumeUnit().trim();
            if (combined.length() > 10) {
                throw new ValidationException("Containers: Combined length of volume and volumeUnit must not exceed 10 characters.");
            }
        }
    }

    private static void validateNetWeightForContainers(TiContainersRequest containersRequest) {
        if ((containersRequest.getNetWeight() != null && StringUtility.isEmpty(containersRequest.getNetWeightUnit())) ||
                (containersRequest.getNetWeight() == null && StringUtility.isNotEmpty(containersRequest.getNetWeightUnit()))) {
            throw new ValidationException("Containers: Net weight and net weight unit must both be provided or both be null.");
        }
        if (containersRequest.getNetWeight() != null && StringUtility.isNotEmpty(containersRequest.getNetWeightUnit())) {
            String combined = containersRequest.getNetWeight().toPlainString() + containersRequest.getNetWeightUnit().trim();
            if (combined.length() > 15) {
                throw new ValidationException("Containers: Combined length of netWeight and netWeightUnit must not exceed 15 characters.");
            }
        }
    }

    private void validateTransportInstructionLegsTruckDriverDetails(PickupDeliveryDetailsRequest request) {
        List<TiLegsRequest> tiLegsList = request.getTiLegsList();
        if (!CollectionUtils.isEmpty(tiLegsList)) {
            for (TiLegsRequest tiLegsRequest : tiLegsList) {
                List<TiTruckDriverDetailsRequest> tiTruckDriverDetails = tiLegsRequest.getTiTruckDriverDetails();
                validateTruckDriverRequest(tiTruckDriverDetails);
            }
        }
    }

    private static void validateTruckDriverRequest(List<TiTruckDriverDetailsRequest> tiTruckDriverDetails) {
        if (!CollectionUtils.isEmpty(tiTruckDriverDetails)) {
            for (TiTruckDriverDetailsRequest truckDriver : tiTruckDriverDetails) {
                if (StringUtility.isEmpty(truckDriver.getDriverName()) && StringUtility.isEmpty(truckDriver.getDriverMobileNumber()) && StringUtility.isEmpty(truckDriver.getTruckOrTrailerType())
                        && StringUtility.isEmpty(truckDriver.getTrailerNumberPlate()) && StringUtility.isEmpty(truckDriver.getTruckNumberPlate())) {
                    throw new ValidationException("Atleast one field is required to save, or please discard by clicking the cancel(x)");
                }
            }
        }
    }

    private void validateTransportInstructionLegsReferenceNumbers(PickupDeliveryDetailsRequest request) {
        List<TiLegsRequest> tiLegsList = request.getTiLegsList();
        if (!CollectionUtils.isEmpty(tiLegsList)) {
            for (TiLegsRequest tiLegsRequest : tiLegsList) {
                List<TiReferencesRequest> tiReferences = tiLegsRequest.getTiReferences();
                validateReferenceNumberRequest(tiReferences);
            }
        }
    }

    private static void validateReferenceNumberRequest(List<TiReferencesRequest> tiReferences) {
        if (!CollectionUtils.isEmpty(tiReferences)) {
            for (TiReferencesRequest tiReferencesRequest : tiReferences) {
                if (StringUtility.isNotEmpty(tiReferencesRequest.getType()) && StringUtility.isEmpty(tiReferencesRequest.getReference())) {
                    throw new ValidationException("Missing Reference when Type is selected");
                } else if (StringUtility.isEmpty(tiReferencesRequest.getType()) && StringUtility.isNotEmpty(tiReferencesRequest.getReference())) {
                    throw new ValidationException("Missing Type when Reference is present");
                } else if (StringUtility.isEmpty(tiReferencesRequest.getType()) && StringUtility.isEmpty(tiReferencesRequest.getReference())) {
                    throw new ValidationException("Missing both type and reference number");
                }
            }
        }
    }

    private void validateTransportInstructionLegs(PickupDeliveryDetailsRequest pickupDeliveryDetails) {
        List<TiLegsRequest> tiLegsList = pickupDeliveryDetails.getTiLegsList();
        if (!CollectionUtils.isEmpty(tiLegsList)) {
            for (TiLegsRequest tiLegsRequest : tiLegsList) {
                PartiesRequest origin = tiLegsRequest.getOrigin();
                PartiesRequest destination = tiLegsRequest.getDestination();
                if (!Objects.isNull(origin)) {
                    validateOrgAndAddress(origin);
                }
                if (!Objects.isNull(destination)) {
                    validateOrgAndAddress(destination);
                }
                validateDates(tiLegsRequest);
            }
        }
    }

    public void validateDates(TiLegsRequest tiLegsRequest) {
        if (tiLegsRequest.getEstimatedPickup() != null && tiLegsRequest.getEstimatedDelivery() != null) {
            if (tiLegsRequest.getEstimatedPickup().isAfter(tiLegsRequest.getEstimatedDelivery())) {
                throw new ValidationException("Estimated Pickup cannot be later than Estimated Delivery.");
            }
            if (tiLegsRequest.getEstimatedDelivery().isBefore(tiLegsRequest.getEstimatedPickup())) {
                throw new ValidationException("Estimated Delivery cannot be earlier than Estimated Pickup.");
            }
        }

        if (tiLegsRequest.getActualPickup() != null && tiLegsRequest.getActualDelivery() != null) {
            if (tiLegsRequest.getActualPickup().isAfter(tiLegsRequest.getActualDelivery())) {
                throw new ValidationException("Actual Pickup cannot be later than Actual Delivery.");
            }
            if (tiLegsRequest.getActualDelivery().isBefore(tiLegsRequest.getActualPickup())) {
                throw new ValidationException("Actual Delivery cannot be earlier than Actual Pickup.");
            }
        }
    }

    private static void validateOrgAndAddress(PartiesRequest origin) {
        boolean hasOrgInfo = StringUtility.isNotEmpty(origin.getOrgId())
                || StringUtility.isNotEmpty(origin.getOrgCode())
                || !CollectionUtils.isEmpty(origin.getOrgData());
        boolean isOrgInfoMissing = StringUtility.isEmpty(origin.getOrgId())
                && StringUtility.isEmpty(origin.getOrgCode())
                && CollectionUtils.isEmpty(origin.getOrgData());
        boolean isAddressInfoMissing = StringUtility.isEmpty(origin.getAddressCode())
                || StringUtility.isEmpty(origin.getAddressId())
                || CollectionUtils.isEmpty(origin.getAddressData());
        boolean hasAddressInfo = StringUtility.isNotEmpty(origin.getAddressCode())
                && StringUtility.isNotEmpty(origin.getAddressId())
                && !CollectionUtils.isEmpty(origin.getAddressData());

        if (hasOrgInfo) {
            if (isAddressInfoMissing) {
                throw new ValidationException("Please provide the origin address info");
            } else if (isOrgInfoMissing) {
                throw new ValidationException("Please provide the complete org details");
            }
        } else if (hasAddressInfo) {
            throw new ValidationException("Please provide the org details");
        }
    }

    private ResponseEntity<IRunnerResponse> createPickupDeliveryDetails(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            String resp = "Request is empty for Pickup Delivery create with Request Id " + LoggerHelper.getRequestIdFromMDC();
            log.debug("Request is empty for Pickup Delivery create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(resp);
        }
        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        try {
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);
            afterSave(pickupDeliveryDetails, true, request);
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(null)
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("Pickup Delivery Details created successfully for Id {} with Request Id {}", pickupDeliveryDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    private void beforeSave(PickupDeliveryDetails entity) {
        Long id = entity.getId();
        // Set proper references
        CommonUtils.emptyIfNull(entity.getTiLegsList()).forEach(leg -> {
            Long legId = leg.getId();
            leg.setPickupDeliveryDetailsId(id);
            CommonUtils.emptyIfNull(leg.getTiReferences()).forEach(i -> i.setTiLegId(legId));
            CommonUtils.emptyIfNull(leg.getTiPackages()).forEach(i -> i.setTiLegId(legId));
            CommonUtils.emptyIfNull(leg.getTiContainers()).forEach(i -> i.setTiLegId(legId));
            CommonUtils.emptyIfNull(leg.getTiTruckDriverDetails()).forEach(i -> i.setTiLegId(legId));
        });
    }

    private void afterSave(PickupDeliveryDetails pickupDeliveryDetails, boolean isCreate, PickupDeliveryDetailsRequest request) throws RunnerException {
        List<Parties> partiesList = request.getPartiesList();
        Long id = pickupDeliveryDetails.getId();

        if (partiesList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(partiesList, Parties.class, isCreate), id, Constants.PICKUP_DELIVERY);
            pickupDeliveryDetails.setPartiesList(updatedParties);
        }
        if (pickupDeliveryDetails.getShipmentId() != null) {
            List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.findByShipmentId(pickupDeliveryDetails.getShipmentId());
            if (!CommonUtils.listIsNullOrEmpty(pickupDeliveryDetailsList)) {
                List<IRunnerResponse> pickupDeliveryDetailsResponses = convertEntityListToDtoList(pickupDeliveryDetailsList, false);
                CompletableFuture.runAsync(masterDataUtils.withMdc(() -> kafkaAsyncService.pushToKafkaTI(pickupDeliveryDetailsResponses, isCreate, pickupDeliveryDetails.getShipmentId())), executorService);
            }
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        return updatePickupDeliveryDetails(commonRequestModel);
    }

    private ResponseEntity<IRunnerResponse> updatePickupDeliveryDetails(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request Id is null for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }

        long id = request.getId();
        Optional<PickupDeliveryDetails> oldEntity = pickupDeliveryDetailsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        pickupDeliveryDetails.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            beforeSave(pickupDeliveryDetails);
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        afterSave(pickupDeliveryDetails, false, request);

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Pickup Delivery list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery list with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<PickupDeliveryDetails>, Pageable> tuple = fetchData(request, PickupDeliveryDetails.class, tableNames);
            Page<PickupDeliveryDetails> pickupDeliveryDetailsPage = pickupDeliveryDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Pickup Delivery list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(pickupDeliveryDetailsPage.getContent(), request.getPopulateRAKC()),
                    pickupDeliveryDetailsPage.getTotalPages(),
                    pickupDeliveryDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery delete with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request Id null for Pickup Delivery delete with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if (!pickupDeliveryDetails.isPresent()) {
                log.debug("pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(pickupDeliveryDetails.get());
            pickupDeliveryDetailsDao.delete(pickupDeliveryDetails.get());

            if (pickupDeliveryDetails.get().getShipmentId() != null) {
                List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.findByShipmentId(pickupDeliveryDetails.get().getShipmentId());
                if (!CommonUtils.listIsNullOrEmpty(pickupDeliveryDetailsList)) {
                    List<IRunnerResponse> pickupDeliveryDetailsResponses = convertEntityListToDtoList(pickupDeliveryDetailsList, false);
                    CompletableFuture.runAsync(masterDataUtils.withMdc(() -> kafkaAsyncService.pushToKafkaTI(pickupDeliveryDetailsResponses, false, pickupDeliveryDetails.get().getShipmentId())), executorService);
                }
            }
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return retrieveById(commonRequestModel, false);
    }

    private ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel, boolean populateRAKC) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery retrieve with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request Id is null for Pickup Delivery retrieve with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if (!pickupDeliveryDetails.isPresent()) {
                log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Pickup Delivery details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            Set<String> addressIds = new HashSet<>();
            Map<String, RAKCDetailsResponse> rakcDetailsMap = new HashMap<>();
            if (Boolean.TRUE.equals(populateRAKC)) {
                getAddressIds(pickupDeliveryDetails.get(), addressIds);
                rakcDetailsMap = commonUtils.getRAKCDetailsMap(addressIds.stream().toList());
            }
            PickupDeliveryDetailsResponse response = convertEntityToDto(pickupDeliveryDetails.get());
            if (Boolean.TRUE.equals(populateRAKC) && !rakcDetailsMap.isEmpty()) {
                this.populateRAKCDetails(response, rakcDetailsMap);
            }
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    private PickupDeliveryDetailsResponse convertEntityToDto(PickupDeliveryDetails pickupDeliveryDetails) {
        return jsonHelper.convertValue(pickupDeliveryDetails, PickupDeliveryDetailsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<PickupDeliveryDetails> lst, Boolean populateRAKC) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        if (CommonUtils.listIsNullOrEmpty(lst))
            return responseList;
        Map<String, RAKCDetailsResponse> rakcDetailsMap;
        if (Boolean.TRUE.equals(populateRAKC)) {
            Set<String> addressIds = new HashSet<>();
            lst.forEach(pickupDeliveryDetails -> getAddressIds(pickupDeliveryDetails, addressIds));
            rakcDetailsMap = commonUtils.getRAKCDetailsMap(addressIds.stream().toList());
        } else {
            rakcDetailsMap = new HashMap<>();
        }
        lst.forEach(pickupDeliveryDetail -> {
            PickupDeliveryDetailsResponse response = convertEntityToDto(pickupDeliveryDetail);
            if (Boolean.TRUE.equals(populateRAKC) && !rakcDetailsMap.isEmpty()) {
                this.populateRAKCDetails(response, rakcDetailsMap);
            }
            responseList.add(response);
        });
        return responseList;
    }

    private void getAddressIds(PickupDeliveryDetails pickupDeliveryDetails, Set<String> addressIds) {
        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getTransporterDetail())) {
            addressIds.add(pickupDeliveryDetails.getTransporterDetail().getAddressId());
        }

        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getSourceDetail())) {
            addressIds.add(pickupDeliveryDetails.getSourceDetail().getAddressId());
        }

        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getDestinationDetail())) {
            addressIds.add(pickupDeliveryDetails.getDestinationDetail().getAddressId());
        }
    }

    public PickupDeliveryDetails convertRequestToEntity(PickupDeliveryDetailsRequest request) {
        return jsonHelper.convertValue(request, PickupDeliveryDetails.class);
    }

    // V2 Endpoints

    // create
    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> createV2(CommonRequestModel commonRequestModel) {
        return createTransportInstruction(commonRequestModel);
    }

    // update
    @Override
    public ResponseEntity<IRunnerResponse> updateV2(CommonRequestModel commonRequestModel) throws RunnerException {
        return updateTransportInstruction(commonRequestModel);
    }

    // list
    @Override
    public ResponseEntity<IRunnerResponse> listV2(CommonRequestModel commonRequestModel) {
        return this.list(commonRequestModel);
    }

    // delete
    @Override
    @Transactional
    public ResponseEntity<IRunnerResponse> deleteV2(CommonRequestModel commonRequestModel) {
        return this.delete(commonRequestModel);
    }

    // retrieve
    @Override
    public ResponseEntity<IRunnerResponse> retrieveByIdV2(CommonRequestModel commonRequestModel, boolean populateRAKC) {
        return this.retrieveById(commonRequestModel, populateRAKC);
    }

    @Override
    public Optional<PickupDeliveryDetails> findById(Long id) {
        return pickupDeliveryDetailsDao.findById(id);
    }

    @Override
    public List<PickupDeliveryDetails> findByShipmentId(Long shipmentId) {
        return pickupDeliveryDetailsDao.findByShipmentId(shipmentId);
    }

    @Override
    public void processDownStreamConsumerData(List<PickupDeliveryDetails> pickupDeliveryDetailsList, Long shipmentId, PushToDownstreamEventDto message, String transactionId) {
        List<IRunnerResponse> pickupDeliveryDetailsResponses = convertEntityListToDtoList(pickupDeliveryDetailsList, false);
        CompletableFuture.runAsync(masterDataUtils.withMdc(() -> kafkaAsyncService.pushToKafkaTI(pickupDeliveryDetailsResponses, isCreateRequest(message), shipmentId)), executorService);
    }

    private static boolean isCreateRequest(PushToDownstreamEventDto message) {
        if (message.getMeta() != null && message.getMeta().getIsCreate() != null) {
            return message.getMeta().getIsCreate();
        }
        return false;
    }


    private void populateRAKCDetails(PickupDeliveryDetailsResponse pickupDeliveryDetails, Map<String, RAKCDetailsResponse> rakcDetailsMap) {
        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getTransporterDetail())) {
            pickupDeliveryDetails.getTransporterDetail().setRAKCDetails(rakcDetailsMap.getOrDefault(pickupDeliveryDetails.getTransporterDetail().getAddressId(), null));
        }

        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getSourceDetail())) {
            pickupDeliveryDetails.getSourceDetail().setRAKCDetails(rakcDetailsMap.getOrDefault(pickupDeliveryDetails.getSourceDetail().getAddressId(), null));
        }

        if (CommonUtils.checkAddressNotNull(pickupDeliveryDetails.getDestinationDetail())) {
            pickupDeliveryDetails.getDestinationDetail().setRAKCDetails(rakcDetailsMap.getOrDefault(pickupDeliveryDetails.getDestinationDetail().getAddressId(), null));
        }
    }
}
