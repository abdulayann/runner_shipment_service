package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MdmConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.CustomerCategoryRates;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Supplier;

import static com.dpw.runner.shipment.services.entity.enums.DateBehaviorType.ESTIMATED;

@Service
@Slf4j
public class ShipmentCommonUtils {

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private ProductIdentifierUtility productEngine;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private  MasterDataUtils masterDataUtils;

    @Autowired
    private V1ServiceUtil v1ServiceUtil;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IShipmentDao shipmentDao;

    public DBOperationType determineOperationTypeAfterApproval(OceanDGStatus dgStatus, Supplier<TaskStatus> statusSupplier) {

        TaskStatus status = statusSupplier.get();

        if (dgStatus == OceanDGStatus.OCEAN_DG_REQUESTED) {
            return status == TaskStatus.APPROVED
                    ? DBOperationType.DG_APPROVE
                    : DBOperationType.DG_REJECT;
        } else if (dgStatus == OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED) {
            return status == TaskStatus.APPROVED
                    ? DBOperationType.COMMERCIAL_APPROVE
                    : DBOperationType.COMMERCIAL_REJECT;
        }

        return DBOperationType.DG_REQUEST;
    }

    public void setExportBrokerForInterBranchConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(shipmentDetails.getAdditionalDetails() != null &&
                !CommonUtils.checkSameParties(consolidationDetails.getSendingAgent(), shipmentDetails.getAdditionalDetails().getExportBroker())) {
            shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
        } else if (shipmentDetails.getAdditionalDetails() == null) {
            shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            shipmentDetails.getAdditionalDetails().setExportBroker(commonUtils.removeIdFromParty(consolidationDetails.getSendingAgent()));
        }
    }

    public void setImportBrokerForInterBranchConsole(ShipmentDetails shipmentDetails, ConsolidationDetails consolidationDetails) {
        if(shipmentDetails.getAdditionalDetails() != null &&
                !CommonUtils.checkSameParties(consolidationDetails.getReceivingAgent(), shipmentDetails.getAdditionalDetails().getImportBroker())) {
            shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
        } else if (shipmentDetails.getAdditionalDetails() == null) {
            shipmentDetails.setAdditionalDetails(new AdditionalDetails());
            shipmentDetails.getAdditionalDetails().setImportBroker(commonUtils.removeIdFromParty(consolidationDetails.getReceivingAgent()));
        }
    }

    public String getShipmentsSerialNumber() {
        // Moving this responsibility to v1 sequnce table to avoid syncing overhead
        return v1Service.getShipmentSerialNumber();
    }

    public String generateCustomHouseBL(ShipmentDetails shipmentDetails) {
        final ShipmentSettingsDetails tenantSettings = commonUtils.getShipmentSettingFromContext();

        String result = null;

        if (shipmentDetails == null && tenantSettings != null && Boolean.TRUE.equals(tenantSettings.getRestrictHblGen())) {
            return null;
        }

        if(shipmentDetails != null) {
            result = shipmentDetails.getHouseBill();
        }

        if (shipmentDetails != null && tenantSettings.getCustomisedSequence()) {
            try {
                result = productEngine.getCustomizedBLNumber(shipmentDetails);
            }
            catch (Exception e) {
                log.error(e.getMessage());
            }
        }

        if(result == null || result.isEmpty()) {
            final String numberGeneration = tenantSettings.getHousebillNumberGeneration() ==  null ? "" : tenantSettings.getHousebillNumberGeneration();
            result = tenantSettings.getHousebillPrefix() ==  null ? "" : tenantSettings.getHousebillPrefix();

            switch(numberGeneration) {
                case "Random" :
                    result += StringUtility.getRandomString(10);
                    break;
                case "Serial" :
                    String serialNumber = getShipmentsSerialNumber();
                    result += serialNumber;
                    break;
                default : result = "";
                    break;
            }
        }

        return result;
    }

    public ShipmentDetailsResponse buildDefaultShipment() {
        var tenantSettings = commonUtils.getShipmentSettingFromContext();

        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        response.setAdditionalDetails(new AdditionalDetailResponse());
        response.setCarrierDetails(new CarrierDetailResponse());
        response.setTransportMode(tenantSettings.getDefaultTransportMode());
        response.setDirection(tenantSettings.getDefaultShipmentType());
        response.setShipmentType(tenantSettings.getDefaultContainerType());
        response.setWeightUnit(tenantSettings.getWeightChargeableUnit());
        response.setVolumeUnit(Constants.VOLUME_UNIT_M3);
        response.setPacksUnit(tenantSettings.getDefaultPackUnit());
        response.setDgPacksUnit(tenantSettings.getDefaultPackUnit());
        response.setStatus(0);
        response.setSource(Constants.SYSTEM);
        response.setCreatedBy(UserContext.getUser().getUsername());
        response.setCustomerCategory(CustomerCategoryRates.CATEGORY_5);
        response.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));
        response.setShipmentCreatedOn(LocalDateTime.now());
        response.setAutoUpdateWtVol(true);
        response.setDateType(ESTIMATED);


        // Populate default department
        response.setDepartment(commonUtils.getAutoPopulateDepartment(
                response.getTransportMode(), response.getDirection(), MdmConstants.SHIPMENT_MODULE
        ));

        setDefaultAgentAndTenant(response);

        // Generate HBL only for SEA + EXP
        if (Constants.TRANSPORT_MODE_SEA.equals(response.getTransportMode()) &&
                Constants.DIRECTION_EXP.equals(response.getDirection())) {
            response.setHouseBill(generateCustomHouseBL(null));
        }

        return response;
    }

    public void addAuditLogContainers(Containers container, Containers prev, Long shipmentId, String operationName) throws RunnerException {
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(container)
                            .prevData(prev)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentId)
                            .operation(operationName).build()
            );
        } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException |
                 NoSuchMethodException e) {
            log.error(e.getMessage());
        }
    }

    public void createAuditLog(ShipmentDetails entity, String oldEntityJsonString, String operation) {
        try {
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(entity)
                            .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ShipmentDetails.class) : null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(entity.getId())
                            .operation(operation).build()
            );
        } catch (Exception e) {
            log.error("Error creating audit service log", e);
        }
    }

    public ResponseEntity<IRunnerResponse> getIdFromGuid(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error(ShipmentConstants.SHIPMENT_RETRIEVE_REQUEST_EMPTY_ERROR, LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getGuid() == null) {
                log.error("Request Guid Id is null for Shipment retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(UUID.fromString(request.getGuid()));
            if (!shipmentDetails.isPresent()) {
                log.debug(ShipmentConstants.SHIPMENT_DETAILS_NULL_FOR_GUID_ERROR, request.getGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Shipment details fetched successfully for Guid {} with Request Id {}", request.getGuid(), LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse(ShipmentDetailsResponse.builder().id(shipmentDetails.get().getId()).build());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public PendingShipmentActionsResponse mapToNotification(
            ConsolidationDetails consolidation,
            Map<Long, ConsoleShipmentMapping> consolShipmentMap,
            Map<String, TenantModel> tenantDataMap,
            Map<String, EntityTransferUnLocations> locationDataMap,
            boolean includeRequestedType) {

        var carrierDetails = Optional.ofNullable(consolidation.getCarrierDetails()).orElse(new CarrierDetails());
        var tenantData = Optional.ofNullable(
                tenantDataMap.get(StringUtility.convertToString(consolidation.getTenantId()))
        ).orElse(new TenantModel());

        var consolShipment = consolShipmentMap.get(consolidation.getId());

        var builder = PendingShipmentActionsResponse.builder()
                .consolId(consolidation.getId())
                .consolidationNumber(consolidation.getReferenceNumber())
                .masterBill(consolidation.getMawb())
                .ata(carrierDetails.getAta())
                .atd(carrierDetails.getAtd())
                .eta(carrierDetails.getEta())
                .etd(carrierDetails.getEtd())
                .pol(Optional.ofNullable(locationDataMap.get(carrierDetails.getOriginPort()))
                        .map(EntityTransferUnLocations::getLookupDesc)
                        .orElse(carrierDetails.getOriginPort()))
                .pod(Optional.ofNullable(locationDataMap.get(carrierDetails.getDestinationPort()))
                        .map(EntityTransferUnLocations::getLookupDesc)
                        .orElse(carrierDetails.getDestinationPort()))
                .lat(consolidation.getLatDate())
                .branch(tenantData.getCode() + " - " + tenantData.getTenantName())
                .branchDisplayName(tenantData.displayName)
                .hazardous(consolidation.getHazardous())
                .requestedBy(consolShipment.getCreatedBy())
                .requestedOn(consolShipment.getCreatedAt());

        // Optional inclusion of requested type
        if (includeRequestedType && Objects.nonNull(consolShipment.getRequestedType())) {
            builder.requestedType(consolShipment.getRequestedType().getV3Description());
        }

        return builder.build();
    }


    protected void setDefaultAgentAndTenant(ShipmentDetailsResponse response) {
        try {
            log.info("Fetching the Tenant Model for Default Shipment");

            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            TenantModel tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
            String currencyCode = tenantModel.currencyCode;
            response.setFreightLocalCurrency(currencyCode);

            List<UnlocationsResponse> unlocationsResponse = masterDataUtils.fetchUnlocationByOneIdentifier(EntityTransferConstants.ID, StringUtility.convertToString(tenantModel.getUnloco()));
            if (!Objects.isNull(unlocationsResponse) && !unlocationsResponse.isEmpty()) {

                EntityTransferAddress entityTransferAddress = commonUtils.getEntityTransferAddress(tenantModel);
                String transpMode = response.getTransportMode();
                if ((Constants.TRANSPORT_MODE_SEA.equals(transpMode)
                        || Constants.TRANSPORT_MODE_RAI.equals(transpMode))
                        && Boolean.TRUE.equals(shipmentSettingsDetails.getIsRunnerV3Enabled())
                        && entityTransferAddress != null) {
                    response.getAdditionalDetails().setPlaceOfIssue(StringUtility.convertToString(entityTransferAddress.getCity()));
                } else {
                    response.getAdditionalDetails().setPlaceOfIssue(unlocationsResponse.get(0).getLocationsReferenceGUID());
                }
                // set place of supply and paid place
                response.getAdditionalDetails().setPlaceOfSupply(unlocationsResponse.get(0).getLocationsReferenceGUID());
                response.getAdditionalDetails().setPaidPlace(unlocationsResponse.get(0).getLocationsReferenceGUID());
            }

            final PartiesResponse partiesResponse = v1ServiceUtil.getDefaultAgentOrg(tenantModel);
            String direction = response.getDirection();
            if(Constants.DIRECTION_EXP.equals(direction)) {
                // populate export broker and origin branch
                response.getAdditionalDetails().setExportBroker(partiesResponse);
                if(partiesResponse.getOrgId() != null && partiesResponse.getAddressId() != null) {
                    Long originBranchId = commonUtils.getReceivingBranch(partiesResponse.getOrgId(), partiesResponse.getAddressId());
                    response.setOriginBranch(originBranchId);
                }

            } else if(Constants.DIRECTION_IMP.equals(direction)) {
                // populate import broker details
                response.getAdditionalDetails().setImportBroker(partiesResponse);
            }
        } catch (Exception e) {
            log.error("Failed in fetching the tenant data from V1 with error : {}", e);
        }
    }


}
