package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.RoutingConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.BulkUpdateRoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.request.UpdateTransportStatusRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingListResponse;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.BulkRoutingResponse;
import com.dpw.runner.shipment.services.dto.v3.response.VesselVoyageMessage;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.NetworkTransferV3Util;
import com.dpw.runner.shipment.services.utils.RoutingValidationUtil;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.RoutingV3Util;
import com.fasterxml.jackson.core.type.TypeReference;
import com.nimbusds.jose.util.Pair;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("java:S6809")
@Service
@Slf4j
public class RoutingsV3Service implements IRoutingsV3Service {
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IRoutingsDao routingsDao;
    @Autowired
    private AuditLogService auditLogService;
    @Autowired
    private RoutingValidationUtil routingValidationUtil;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private RoutingV3Util routingV3Util;
    @Autowired
    private IShipmentServiceV3 shipmentServiceV3;
    @Autowired
    private IConsolidationV3Service consolidationV3Service;
    @Autowired
    private ICarrierDetailsDao carrierDetailsDao;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Autowired
    private DependentServiceHelper dependentServiceHelper;
    @Autowired
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Autowired
    private NetworkTransferV3Util networkTransferV3Util;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ParentResult {
        private String parent;
        private Long parentId;
    }


    @Transactional
    @Override
    public RoutingsResponse create(CommonRequestModel commonRequestModel, String module) throws RunnerException {
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        if (request == null) {
            String resp = "Request is null for Routing Create";
            log.error("Request is null for Routing Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(resp);
        }
        routingValidationUtil.validateModule(request, module);
        Routings routings = convertRequestToEntity(request);
        try {
            routings = routingsDao.save(routings);

            ParentResult parentResult = getParentDetails(List.of(routings), request.getEntityId(), module);
            // Audit logs
            recordAuditLogs(null, List.of(routings), DBOperationType.CREATE, parentResult);
            VesselVoyageMessage vesselVoyageMessage = new VesselVoyageMessage();
            // afterSave
            afterSave(Arrays.asList(routings), request.getEntityId(), module, TransportInfoStatus.YES, vesselVoyageMessage);
            log.info("Routing created successfully for Id {} with Request Id {}", routings.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
        return convertEntityToDto(routings);
    }

    @Transactional
    public void afterSave(List<Routings> routingList, Long entityId, String module, TransportInfoStatus transportInfoStatus, VesselVoyageMessage vesselVoyageMessage) throws RunnerException {
        List<Routings> mainCarriageList = routingList.stream()
                .filter(routing -> routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE)
                .toList();
        if (Constants.SHIPMENT.equalsIgnoreCase(module)) {
            Long shipmentId = entityId;
            if (entityId == null) {
                shipmentId = mainCarriageList.get(0).getShipmentId();
            }
            updateShipmentCarrierDetails(mainCarriageList, shipmentId, transportInfoStatus, vesselVoyageMessage);
        } else if (Constants.CONSOLIDATION.equalsIgnoreCase(module)) {
            //updates routings to attached shipments
            Long consolidationId = entityId;
            if (entityId == null) {
                consolidationId = mainCarriageList.get(0).getConsolidationId();
            }
            ConsolidationDetails consolidationDetails = consolidationV3Service.getConsolidationById(consolidationId);
            vesselVoyageMessage.setMessage(getMessageValue(mainCarriageList, consolidationDetails.getCarrierDetails()));
            commonUtils.validateAirSecurityAndDGConsolidationPermissions(consolidationDetails);
            updateConsolCarrierDetails(mainCarriageList, consolidationDetails, transportInfoStatus);
            Set<ShipmentDetails> shipmentsList = consolidationDetails.getShipmentsList();

            if (isInterBranchContextNeeded(consolidationDetails))
                commonUtils.setInterBranchContextForHub();

            if (!CollectionUtils.isEmpty(shipmentsList)) {
                for (ShipmentDetails shipmentDetails : shipmentsList) {
                    List<Routings> originalRoutings = shipmentDetails.getRoutingsList();
                    List<Routings> updatedRoutings = new ArrayList<>(originalRoutings);

                    // Step 1: Collect indices of inherited MAIN_CARRIAGE to be removed
                    List<Integer> inheritedIndexes = new ArrayList<>();
                    getMainCarriageInheritedIndex(updatedRoutings, inheritedIndexes);

                    // Step 2: Remove those inherited MAIN_CARRIAGE entries
                    removeMainCarriageFromShipmentRouting(mainCarriageList, updatedRoutings);


                    // Step 3: Prepare new routings from consolidated MAIN_CARRIAGE
                    List<Routings> consolidatedMainCarriages = mainCarriageList.stream()
                            .filter(r -> r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE)
                            .map(consolRouting -> cloneRoutingForShipment(consolRouting, shipmentDetails.getId()))
                            .toList();

                    // Step 4: Insert new consolidated MAIN_CARRIAGE routings at the inheritedIndexes or end
                    int offset = 0;
                    insertNewConsolMainCarriageAtInheritedIndex(updatedRoutings, inheritedIndexes, consolidatedMainCarriages, offset);

                    // Step 5: Push to update
                    BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
                    bulkUpdateRoutingsRequest.setRoutings(jsonHelper.convertValueToList(updatedRoutings, RoutingsRequest.class));
                    bulkUpdateRoutingsRequest.setEntityId(shipmentDetails.getId());
                    bulkUpdateRoutingsRequest.setTransportInfoStatus(TransportInfoStatus.IH);
                    updateBulk(bulkUpdateRoutingsRequest, Constants.SHIPMENT);
                }
            }
        }
    }

    private String getMessageValue(List<Routings> mainCarriageList, CarrierDetails carrierDetails) {
        if (!CollectionUtils.isEmpty(mainCarriageList)) {
            Routings mainCarriageLeg = mainCarriageList.get(0);
            if (mainCarriageList.size() > 1) {
                Optional<Routings> routings = mainCarriageList.stream().filter(r -> Boolean.TRUE.equals(r.getIsSelectedForDocument())).findFirst();
                if (routings.isPresent()) {
                    mainCarriageLeg = routings.get();
                }
            }
            StringBuilder description = new StringBuilder();
            checkIfVoyageVesselOrFlightNumberChanged(carrierDetails, mainCarriageLeg, description);
            if (StringUtility.isNotEmpty(mainCarriageLeg.getCarrier()) && !Objects.equals(mainCarriageLeg.getCarrier(), carrierDetails.getShippingLine())) {
                if (StringUtility.isEmpty(description.toString())) {
                    description.append("Carrier");
                } else {
                    description.append("/Carrier");
                }
            }
            if (!StringUtility.isEmpty(description.toString())) {
                description.append(" are updated as per Routing Leg");
                return "Route(s) updated successfully. " + description;
            }
        }
        return Constants.EMPTY_STRING;
    }

    private static void checkIfVoyageVesselOrFlightNumberChanged(CarrierDetails carrierDetails, Routings mainCarriageLeg, StringBuilder description) {
        if (StringUtility.isNotEmpty(mainCarriageLeg.getVesselName()) && !Objects.equals(mainCarriageLeg.getVesselName(), carrierDetails.getVessel())) {
            description.append("Vessel");
        }
        if (StringUtility.isNotEmpty(mainCarriageLeg.getVoyage()) && !Objects.equals(mainCarriageLeg.getVoyage(), carrierDetails.getVoyage())) {
            if (StringUtility.isEmpty(description.toString())) {
                description.append("Voyage");
            } else {
                description.append("/Voyage");
            }

        }
        if (StringUtility.isNotEmpty(mainCarriageLeg.getFlightNumber()) && !Objects.equals(mainCarriageLeg.getFlightNumber(), carrierDetails.getFlightNumber())) {
            if (StringUtility.isEmpty(description.toString())) {
                description.append("Flight Number");
            } else {
                description.append("/Flight Number");
            }

        }
    }

    private void updateShipmentCarrierDetails(List<Routings> mainCarriageList, Long shipmentId, TransportInfoStatus transportInfoStatus, VesselVoyageMessage vesselVoyageMessage) {
        Optional<ShipmentDetails> shipmentDetailsOptional = shipmentServiceV3.findById(shipmentId);
        if (shipmentDetailsOptional.isEmpty())
            return;
        ShipmentDetails shipmentDetails = shipmentDetailsOptional.get();
        vesselVoyageMessage.setMessage(getMessageValue(mainCarriageList, shipmentDetails.getCarrierDetails()));
        if (!CollectionUtils.isEmpty(mainCarriageList)) {
            updateShipmentCarrierDetailsFromMainCarriage(mainCarriageList, shipmentDetails, transportInfoStatus);
        } else {
            shipmentDetails.getCarrierDetails().setAtd(null);
            shipmentDetails.getCarrierDetails().setAta(null);
            shipmentDetails.getCarrierDetails().setEta(null);
            shipmentDetails.getCarrierDetails().setEtd(null);
        }
    }

    private void updateConsolCarrierDetails(List<Routings> mainCarriageList, ConsolidationDetails consolidationDetails, TransportInfoStatus transportInfoStatus) {
        if (!CollectionUtils.isEmpty(mainCarriageList)) {
            updateConsolCarrierDetails(mainCarriageList, transportInfoStatus);
        } else {
            consolidationDetails.getCarrierDetails().setAtd(null);
            consolidationDetails.getCarrierDetails().setAta(null);
            consolidationDetails.getCarrierDetails().setEta(null);
            consolidationDetails.getCarrierDetails().setEtd(null);
        }
    }

    private static void insertNewConsolMainCarriageAtInheritedIndex(List<Routings> updatedRoutings, List<Integer> inheritedIndexes, List<Routings> consolidatedMainCarriages, int offset) {
        for (int i = 0; i < consolidatedMainCarriages.size(); i++) {
            int insertAt = i < inheritedIndexes.size()
                    ? inheritedIndexes.get(i)
                    : offset; // append to end if more than removed
            if (insertAt >= updatedRoutings.size()) {
                updatedRoutings.add(consolidatedMainCarriages.get(i));
            } else {
                updatedRoutings.add(insertAt, consolidatedMainCarriages.get(i));
            }
            offset = insertAt + 1;
        }
    }

    private static void removeMainCarriageFromShipmentRouting(List<Routings> mainCarriageList, List<Routings> updatedRoutings) {
        boolean isMainCarriagePresent = updatedRoutings.stream()
                .anyMatch(r -> r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE &&
                        Boolean.TRUE.equals(r.getInheritedFromConsolidation()));
        if (!isMainCarriagePresent && !CollectionUtils.isEmpty(mainCarriageList)) {
            updatedRoutings.removeIf(r -> r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE);
        }
        updatedRoutings.removeIf(r -> r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE &&
                Boolean.TRUE.equals(r.getInheritedFromConsolidation()));
    }

    private static void getMainCarriageInheritedIndex(List<Routings> updatedRoutings, List<Integer> inheritedIndexes) {
        for (int i = 0; i < updatedRoutings.size(); i++) {
            Routings routing = updatedRoutings.get(i);
            if (routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE &&
                    Boolean.TRUE.equals(routing.getInheritedFromConsolidation())) {
                inheritedIndexes.add(i);
            }
        }
    }

    private void updateConsolCarrierDetails(List<Routings> mainCarriageList, TransportInfoStatus transportInfoStatus) {
        if (!CollectionUtils.isEmpty(mainCarriageList)) {
            updateConsolidationCarrierDetailsFromMainCarriage(mainCarriageList, transportInfoStatus);
        }
    }

    private CarrierDetails getNewCarrierDetails(CarrierDetails currentCarrierDetails) {
        CarrierDetails existingCarrierDetails = null;

        if (currentCarrierDetails != null) {
            existingCarrierDetails = new CarrierDetails();
            existingCarrierDetails.setEta(currentCarrierDetails.getEta());
            existingCarrierDetails.setEtd(currentCarrierDetails.getEtd());
            existingCarrierDetails.setAta(currentCarrierDetails.getAta());
            existingCarrierDetails.setAtd(currentCarrierDetails.getAtd());
        }
        return existingCarrierDetails;
    }

    /**
     * Updates shipment's carrier details from main carriage routing legs based on tenantSettings
     */
    private void updateShipmentCarrierDetailsFromMainCarriage(List<Routings> mainCarriageRoutings, ShipmentDetails shipmentDetails, TransportInfoStatus transportInfoStatus) {
        commonUtils.validateAirSecurityAndDGShipmentPermissions(shipmentDetails);
        CarrierDetails existingCarrierDetails = getNewCarrierDetails(shipmentDetails.getCarrierDetails());
        updateCarrierDetails(shipmentDetails, mainCarriageRoutings, existingCarrierDetails, transportInfoStatus);
        carrierDetailsDao.update(shipmentDetails.getCarrierDetails());
    }

    private void updateConsolidationCarrierDetailsFromMainCarriage(List<Routings> mainCarriageRoutings, TransportInfoStatus transportInfoStatus) {
        Optional<ConsolidationDetails> consolidationDetailsOptional = consolidationV3Service.findById(mainCarriageRoutings.get(0).getConsolidationId());
        if (consolidationDetailsOptional.isEmpty()) return;
        ConsolidationDetails consolidationDetails = consolidationDetailsOptional.get();
        CarrierDetails existingCarrierDetails = getNewCarrierDetails(consolidationDetails.getCarrierDetails());
        updateCarrierDetails(consolidationDetails, mainCarriageRoutings, existingCarrierDetails, transportInfoStatus);
        carrierDetailsDao.update(consolidationDetails.getCarrierDetails());
    }

    private boolean isValueChanged(Object newValue, Object oldValue) {
        return (oldValue != null && newValue == null) || (newValue != null && !newValue.equals(oldValue));
    }

    boolean isValidDateChange(CarrierDetails newCarrierDetails, CarrierDetails oldCarrierDetails) {
        if (oldCarrierDetails == null && newCarrierDetails != null) {
            return newCarrierDetails.getEta() != null
                    || newCarrierDetails.getEtd() != null
                    || newCarrierDetails.getAta() != null
                    || newCarrierDetails.getAtd() != null;
        }
        if (oldCarrierDetails != null && newCarrierDetails != null) {
            return isValueChanged(newCarrierDetails.getEta(), oldCarrierDetails.getEta())
                    || isValueChanged(newCarrierDetails.getEtd(), oldCarrierDetails.getEtd())
                    || isValueChanged(newCarrierDetails.getAta(), oldCarrierDetails.getAta())
                    || isValueChanged(newCarrierDetails.getAtd(), oldCarrierDetails.getAtd());
        }
        return false;
    }

    /**
     * Updates the CarrierDetails fields using first and last main carriage legs.
     */
    private void updateCarrierDetails(ConsolidationDetails consolidationDetails, List<Routings> mainCarriageRoutings, CarrierDetails existingCarrierDetails, TransportInfoStatus transportInfoStatus) {
        CarrierDetails carrierDetails = consolidationDetails.getCarrierDetails();
        Routings firstLeg = mainCarriageRoutings.get(0);
        Routings lastLeg = mainCarriageRoutings.get(mainCarriageRoutings.size() - 1);
        if (Constants.TRANSPORT_MODE_SEA.equals(consolidationDetails.getTransportMode())) {
            updateVesselAndVoyage(mainCarriageRoutings, carrierDetails, firstLeg);
        } else if (Constants.TRANSPORT_MODE_AIR.equals(consolidationDetails.getTransportMode())) {
            setCarrierAndFlightNumberForAir(carrierDetails, firstLeg);
        }
        if (TransportInfoStatus.YES.equals(transportInfoStatus) || TransportInfoStatus.IH.equals(transportInfoStatus)) {
            if (TransportInfoStatus.IH.equals(transportInfoStatus)) {
                if (Objects.equals(firstLeg.getPol(), carrierDetails.getOriginPort()) && Objects.equals(lastLeg.getPod(), carrierDetails.getDestinationPort())) {
                    consolidationDetails.setTransportInfoStatus(TransportInfoStatus.YES);
                    updateCarrierDetailsPolAndPod(carrierDetails, firstLeg, lastLeg);
                }
            } else {
                updateCarrierDetailsPolAndPod(carrierDetails, firstLeg, lastLeg);
            }
        }

        carrierDetails.setEtd(firstLeg.getEtd());
        carrierDetails.setAtd(firstLeg.getAtd());
        carrierDetails.setEta(lastLeg.getEta());
        carrierDetails.setAta(lastLeg.getAta());
        if (isValidDateChange(carrierDetails, existingCarrierDetails))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(consolidationDetails, null, true)));
    }

    private static void updateCarrierDetailsPolAndPod(CarrierDetails carrierDetails, Routings firstLeg, Routings lastLeg) {
        if (!StringUtility.isEmpty(firstLeg.getPol())) {
            carrierDetails.setOriginPort(firstLeg.getPol());
            carrierDetails.setOriginPortLocCode(firstLeg.getOriginPortLocCode());
        }
        if (!StringUtility.isEmpty(lastLeg.getPod())) {
            carrierDetails.setDestinationPort(lastLeg.getPod());
            carrierDetails.setDestinationPortLocCode(lastLeg.getDestinationPortLocCode());
        }
    }

    /**
     * Updates the CarrierDetails fields using first and last main carriage legs.
     */
    private void updateCarrierDetails(ShipmentDetails shipmentDetails, List<Routings> mainCarriageRoutings, CarrierDetails existingCarrierDetails, TransportInfoStatus transportInfoStatus) {
        CarrierDetails carrierDetails = shipmentDetails.getCarrierDetails();
        Routings firstLeg = mainCarriageRoutings.get(0);
        Routings lastLeg = mainCarriageRoutings.get(mainCarriageRoutings.size() - 1);

        updateCarrierDetailsBasedOnTransportInfoStatus(shipmentDetails, mainCarriageRoutings, transportInfoStatus, carrierDetails, firstLeg, lastLeg);
        if (Boolean.TRUE.equals(carrierDetails.getIsSameAsOriginPort())) {
            carrierDetails.setOrigin(carrierDetails.getOriginPort());
            carrierDetails.setOriginLocCode(carrierDetails.getOriginPortLocCode());
            carrierDetails.setOriginCountry(carrierDetails.getOriginPortCountry());
        }

        if (Boolean.TRUE.equals(carrierDetails.getIsSameAsDestinationPort())) {
            carrierDetails.setDestination(carrierDetails.getDestinationPort());
            carrierDetails.setDestinationLocCode(carrierDetails.getDestinationPortLocCode());
            carrierDetails.setDestinationCountry(carrierDetails.getDestinationPortCountry());
        }
        carrierDetails.setEtd(firstLeg.getEtd());
        carrierDetails.setAtd(firstLeg.getAtd());
        carrierDetails.setEta(lastLeg.getEta());
        carrierDetails.setAta(lastLeg.getAta());

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if (shipmentSettingsDetails != null && Boolean.TRUE.equals(shipmentSettingsDetails.getIsAutomaticTransferEnabled()) && isValidDateChange(carrierDetails, existingCarrierDetails))
            CompletableFuture.runAsync(masterDataUtils.withMdc(() -> networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails, null, true)));
    }

    private static void updateCarrierDetailsBasedOnTransportInfoStatus(ShipmentDetails shipmentDetails, List<Routings> mainCarriageRoutings, TransportInfoStatus transportInfoStatus, CarrierDetails carrierDetails, Routings firstLeg, Routings lastLeg) {
        if (Constants.TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode())) {
            shipmentDetails.setTransportInfoStatus(transportInfoStatus);
            updateVesselAndVoyage(mainCarriageRoutings, carrierDetails, firstLeg);
        } else if (Constants.TRANSPORT_MODE_AIR.equals(shipmentDetails.getTransportMode())) {
            setCarrierAndFlightNumberForAir(carrierDetails, firstLeg);
        }
        if (TransportInfoStatus.YES.equals(transportInfoStatus) || TransportInfoStatus.IH.equals(transportInfoStatus)) {
            if (TransportInfoStatus.IH.equals(transportInfoStatus)) {
                if (Objects.equals(firstLeg.getPol(), carrierDetails.getOriginPort()) && Objects.equals(lastLeg.getPod(), carrierDetails.getDestinationPort())) {
                    shipmentDetails.setTransportInfoStatus(TransportInfoStatus.YES);
                    updateCarrierDetailsPolAndPod(carrierDetails, firstLeg, lastLeg);
                }
            } else {
                updateCarrierDetailsPolAndPod(carrierDetails, firstLeg, lastLeg);
            }
        }
    }


    private static void updateVesselAndVoyage(List<Routings> mainCarriageRoutings, CarrierDetails carrierDetails, Routings mainCarriageLeg) {
        if (mainCarriageRoutings.size() > 1) {
            Optional<Routings> routings = mainCarriageRoutings.stream().filter(r -> Boolean.TRUE.equals(r.getIsSelectedForDocument())).findFirst();
            if (routings.isPresent()) {
                mainCarriageLeg = routings.get();
            }
        }

        if (StringUtility.isNotEmpty(mainCarriageLeg.getVesselName())) {
            carrierDetails.setVessel(mainCarriageLeg.getVesselName());
        }
        if (StringUtility.isNotEmpty(mainCarriageLeg.getVoyage())) {
            carrierDetails.setVoyage(mainCarriageLeg.getVoyage());
        }
        if (StringUtility.isNotEmpty(mainCarriageLeg.getCarrier())) {
            carrierDetails.setShippingLine(mainCarriageLeg.getCarrier());
        }
    }

    private static void setCarrierAndFlightNumberForAir(CarrierDetails carrierDetails, Routings mainCarriageLeg) {
        if (StringUtility.isNotEmpty(mainCarriageLeg.getFlightNumber())) {
            carrierDetails.setFlightNumber(mainCarriageLeg.getFlightNumber());
        }
        if (StringUtility.isNotEmpty(mainCarriageLeg.getCarrier())) {
            carrierDetails.setShippingLine(mainCarriageLeg.getCarrier());
        }
    }

    @Override
    @Transactional
    public RoutingsResponse update(CommonRequestModel commonRequestModel, String module) throws RunnerException {
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        routingValidationUtil.validateUpdateRequest(request);
        routingValidationUtil.validateModule(request, module);
        Optional<Routings> oldEntity = routingsDao.findById(request.getId());
        if (oldEntity.isEmpty()) {
            log.debug(RoutingConstants.ROUTING_NULL_FOR_ID_ERROR, request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Routings oldEntityData = jsonHelper.convertValue(oldEntity.get(), Routings.class);
        Routings routings = convertRequestToEntity(request);
        try {
            routings = routingsDao.save(routings);

            ParentResult parentResult = getParentDetails(List.of(routings), request.getEntityId(), module);
            // Audit logs
            recordAuditLogs(List.of(oldEntityData), List.of(routings), DBOperationType.UPDATE, parentResult);
            VesselVoyageMessage vesselVoyageMessage = new VesselVoyageMessage();
            // afterSave operations
            afterSave(Arrays.asList(routings), request.getEntityId(), module, TransportInfoStatus.YES, vesselVoyageMessage);
            log.info("Routing updated successfully for Id {} with Request Id {}", routings.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
        return convertEntityToDto(routings);
    }


    @Override
    public RoutingListResponse list(ListCommonRequest request, String xSource) throws RunnerException {
        String responseMsg;
        try {
            if (request == null) {
                log.error("Request is empty for Routing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new RunnerException("Request is empty for Routing list");
            }
            Pair<Specification<Routings>, Pageable> tuple = fetchData(request, Routings.class);
            Page<Routings> routingsPage;
            if (Objects.equals(xSource, NETWORK_TRANSFER))
                routingsPage = routingsDao.findAllWithoutTenantFilter(tuple.getLeft(), tuple.getRight());
            else
                routingsPage = routingsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Routing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            List<RoutingsResponse> response = convertEntityListToDtoList(routingsPage.getContent());
            Map<String, Object> masterData = this.getMasterDataForList(response);
            return RoutingListResponse.builder().routings(response).totalCount(routingsPage.getTotalElements())
                    .totalPages(routingsPage.getTotalPages()).masterData(masterData).build();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    Map<String, Object> getMasterDataForList(List<RoutingsResponse> response) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        try {
            double startTime = System.currentTimeMillis();
            var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllUnlocationInSingleCallList(response, masterDataResponse)), executorServiceMasterData);
            var masterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllMasterDataInSingleCallList(response, masterDataResponse)), executorServiceMasterData);
            var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllVesselInSingleCallList(response, masterDataResponse)), executorServiceMasterData);
            CompletableFuture.allOf(locationDataFuture, masterDataFuture, vesselDataFuture).join();
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.ROUTING_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_ROUTING_LIST, ex.getLocalizedMessage());
        }
        return masterDataResponse;
    }

    @Override
    public void delete(CommonRequestModel commonRequestModel, String module) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        routingValidationUtil.validateDeleteRequest(request);
        Optional<Routings> routing = routingsDao.findById(request.getId());
        if (routing.isEmpty()) {
            log.debug(RoutingConstants.ROUTING_NULL_FOR_ID_ERROR, request.getId());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        Routings oldEntityData = jsonHelper.convertValue(routing.get(), Routings.class);
        try {
            routingsDao.delete(routing.get());

            ParentResult parentResult = getParentDetails(List.of(oldEntityData), request.getId(), module);
            // Audit logs
            recordAuditLogs(List.of(oldEntityData), null, DBOperationType.DELETE, parentResult);
            log.info("Routing deleted successfully for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    @Override
    public RoutingsResponse retrieveById(CommonRequestModel commonRequestModel, String xSource) throws RunnerException {
        CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
        if (request.getId() == null) {
            log.error(RoutingConstants.ROUTING_ID_NULL_FOR_RETRIEVE, LoggerHelper.getRequestIdFromMDC());
            throw new RunnerException(RoutingConstants.ID_GUID_NULL_ERROR);
        }
        Optional<Routings> routings;
        if (Objects.equals(xSource, NETWORK_TRANSFER))
            routings = routingsDao.findByIdWithQuery(request.getId());
        else
            routings = routingsDao.findById(request.getId());
        if (routings.isEmpty()) {
            log.debug("Routing is null for the input id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        return convertEntityToDto(routings.get());
    }

    private Routings convertRequestToEntity(RoutingsRequest request) {
        return jsonHelper.convertValue(request, Routings.class);
    }

    private RoutingsResponse convertEntityToDto(Routings routings) {
        return jsonHelper.convertValue(routings, RoutingsResponse.class);
    }

    private List<RoutingsResponse> convertEntityListToDtoList(List<Routings> lst) {
        List<RoutingsResponse> routingsListResponses = new ArrayList<>();
        lst.forEach(route -> {
            var response = modelMapper.map(route, RoutingsResponse.class);
            routingsListResponses.add(response);
        });
        return routingsListResponses;
    }

    @Override
    @Transactional
    public BulkRoutingResponse updateBulk(BulkUpdateRoutingsRequest request, String module) throws RunnerException {
        routingValidationUtil.validateBulkUpdateRoutingRequest(request, module);
        List<RoutingsRequest> incomingRoutings = request.getRoutings();
        setFlightNumberInCaseAir(incomingRoutings);
        // Separate IDs and determine existing routing
        List<Long> incomingIds = getIncomingRoutingsIds(incomingRoutings);
        List<Routings> existingRoutings = new ArrayList<>();
        List<Routings> oldConvertedRouting = null;
        if (!CollectionUtils.isEmpty(incomingIds)) {
            existingRoutings = routingsDao.findByIdIn(incomingIds);
            // Validate incoming request
            routingValidationUtil.validateUpdateBulkRequest(incomingRoutings, existingRoutings);
            oldConvertedRouting = jsonHelper.convertValueToList(existingRoutings, Routings.class);
        }
        List<Routings> oldRoutingsForDeletion = deleteOrphanRoutings(request, module);

        List<Routings> routingsList = reOrderRoutings(jsonHelper.convertValueToList(incomingRoutings, Routings.class), existingRoutings);
        // Separate into create and update requests

        List<Routings> allSavedRouting = routingsDao.saveAll(routingsList);

        ParentResult parentResult = getParentDetails(allSavedRouting, request.getEntityId(), module);
        List<Routings> matchedIncomingRoutings = new ArrayList<>();
        List<Routings> newRoutings = new ArrayList<>();

        for (Routings routing : allSavedRouting) {
            if (incomingIds.contains(routing.getId())) {
                matchedIncomingRoutings.add(routing);
            } else {
                newRoutings.add(routing);
            }
        }
        // Audit logs
        if (!CollectionUtils.isEmpty(oldConvertedRouting)) {
            recordAuditLogs(oldConvertedRouting, matchedIncomingRoutings, DBOperationType.UPDATE, parentResult);
        }
        if (!CollectionUtils.isEmpty(newRoutings)) {
            recordAuditLogs(null, newRoutings, DBOperationType.CREATE, parentResult);
        }
        if (!CollectionUtils.isEmpty(oldRoutingsForDeletion)) {
            recordAuditLogs(oldRoutingsForDeletion, null, DBOperationType.DELETE, parentResult);
        }

        // Convert to response
        List<RoutingsResponse> routingResponses = jsonHelper.convertValueToList(allSavedRouting, RoutingsResponse.class);
        VesselVoyageMessage vesselVoyageMessage = new VesselVoyageMessage();
        afterSave(allSavedRouting, request.getEntityId(), module, request.getTransportInfoStatus(), vesselVoyageMessage);

        // Triggering Event for shipment and console for DependentServices update
        pushToDependentServices(request, module, allSavedRouting);

        return BulkRoutingResponse.builder()
                .routingsResponseList(routingResponses)
                .message(prepareBulkUpdateMessage(routingResponses, vesselVoyageMessage))
                .build();
    }

    private void setFlightNumberInCaseAir(List<RoutingsRequest> incomingRoutings) {
        if (!CollectionUtils.isEmpty(incomingRoutings)) {
            for (RoutingsRequest routingsRequest : incomingRoutings) {
                if (routingsRequest.getCarriage() == RoutingCarriage.MAIN_CARRIAGE && Constants.TRANSPORT_MODE_AIR.equals(routingsRequest.getMode())) {
                    routingsRequest.setFlightNumber(routingsRequest.getVoyage());
                    routingsRequest.setVoyage(null);
                }
            }
        }
    }

    private void pushToDependentServices(BulkUpdateRoutingsRequest request, String module, List<Routings> routingsList) {
        if (Objects.equals(module, SHIPMENT)) {
            Long shipId = request.getEntityId() != null ? request.getEntityId() : routingsList.get(0).getShipmentId();
            triggerPushToDownStreamForShipment(shipId);
        }
    }

    private void triggerPushToDownStreamForShipment(Long shipmentId) {
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(shipmentId)
                .parentEntityName(SHIPMENT)
                .build();
        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, shipmentId.toString());
    }

    @NotNull
    private static List<Long> getIncomingRoutingsIds(List<RoutingsRequest> incomingRoutings) {
        return incomingRoutings.stream()
                .map(RoutingsRequest::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();
    }

    private List<Routings> deleteOrphanRoutings(BulkUpdateRoutingsRequest request, String module) {
        List<Long> incomingRoutingsIds = getIncomingRoutingsIds(request.getRoutings());
        List<Routings> existingRoutingsForDeletion = getExistingRoutingByModule(request.getRoutings(), request.getEntityId(), module);
        List<Routings> routingsToDelete = existingRoutingsForDeletion.stream()
                .filter(routing -> !incomingRoutingsIds.contains(routing.getId()))
                .toList();
        if (!CollectionUtils.isEmpty(routingsToDelete)) {
            List<Routings> oldRoutingsForDeletion = jsonHelper.convertValueToList(routingsToDelete, Routings.class);
            routingsDao.deleteAll(routingsToDelete);
            return oldRoutingsForDeletion;
        }
        return new ArrayList<>();
    }

    private List<Routings> getExistingRoutingByModule(List<RoutingsRequest> incomingRoutings, Long entityId, String module) {
        if (Constants.SHIPMENT.equals(module)) {
            if (CollectionUtils.isEmpty(incomingRoutings)) {
                return routingsDao.findByShipmentId(entityId);
            }
            return routingsDao.findByShipmentId(incomingRoutings.get(0).getShipmentId());
        } else if (Constants.CONSOLIDATION.equals(module)) {
            if (CollectionUtils.isEmpty(incomingRoutings)) {
                return routingsDao.findByConsolidationId(entityId);
            }
            return routingsDao.findByConsolidationId(incomingRoutings.get(0).getConsolidationId());
        }
        return new ArrayList<>();
    }

    private void recordAuditLogs(List<Routings> oldRouting, List<Routings> newRouting, DBOperationType operationType, ParentResult parentResult) {
        Map<Long, Routings> oldRoutingMap = Optional.ofNullable(oldRouting).orElse(List.of()).stream()
                .filter(route -> route.getId() != null)
                .collect(Collectors.toMap(Routings::getId, Function.identity(),
                        (existing, replacement) -> existing));

        Map<Long, Routings> newRoutingMap = Optional.ofNullable(newRouting).orElse(List.of()).stream()
                .filter(route -> route.getId() != null)
                .collect(Collectors.toMap(Routings::getId, Function.identity(),
                        (existing, replacement) -> existing));

        // Decide the relevant set of IDs based on operation
        Set<Long> idsToProcess = switch (operationType) {
            case CREATE -> newRoutingMap.keySet();
            case DELETE -> oldRoutingMap.keySet();
            case UPDATE -> {
                Set<Long> ids = new HashSet<>(oldRoutingMap.keySet());
                ids.retainAll(newRoutingMap.keySet()); // only intersecting IDs
                yield ids;
            }
            default -> throw new IllegalStateException("Unexpected value: " + operationType);
        };

        for (Long id : idsToProcess) {
            try {
                Routings oldData = oldRoutingMap.get(id);
                Routings newData = newRoutingMap.get(id);

                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId())
                                .userName(UserContext.getUser().getUsername())
                                .prevData(oldData)
                                .newData(newData)
                                .parent(parentResult.getParent())
                                .parentId(parentResult.getParentId())
                                .operation(operationType.name())
                                .build()
                );
            } catch (Exception ex) {
                log.error("Failed to add audit log for Routing ID {} and operation [{}]: {}", id, operationType, ex.getMessage(), ex);
            }
        }
    }

    public ParentResult getParentDetails(List<Routings> routingList, Long entityId, String moduleType) {
        Long parentId = entityId;
        Routings firstRouting = null;
        if (!CollectionUtils.isEmpty(routingList)) {
            firstRouting = routingList.get(0);
        }

        return switch (moduleType) {
            case Constants.SHIPMENT ->
                    new ParentResult(ShipmentDetails.class.getSimpleName(), entityId == null ? firstRouting.getShipmentId() : parentId);
            case Constants.CONSOLIDATION ->
                    new ParentResult(ConsolidationDetails.class.getSimpleName(), entityId == null ? firstRouting.getConsolidationId() : parentId);
            case Constants.BOOKING ->
                    new ParentResult(CustomerBooking.class.getSimpleName(), entityId == null ? firstRouting.getBookingId() : parentId);
            default -> throw new IllegalArgumentException("Unsupported module type: " + moduleType);
        };
    }

    private String prepareBulkUpdateMessage(List<RoutingsResponse> routingResponses, VesselVoyageMessage vesselVoyageMessage) {
        String message;
        if (StringUtility.isNotEmpty(vesselVoyageMessage.getMessage())) {
            return vesselVoyageMessage.getMessage();
        }
        // If more than one Route was updated, return a generic bulk success message
        if (routingResponses.size() > 1) {
            message = "Bulk edit success! All selected routes have been updated.";
        } else {
            message = "Routing saved successfully.";
        }

        return message;
    }

    @Override
    @Transactional
    public BulkRoutingResponse deleteBulk(List<RoutingsRequest> routingListRequest, String module) throws RunnerException {
        routingValidationUtil.validateDeleteBulkRequest(routingListRequest);
        // Extract unique routing IDs from the request
        List<Long> routingIds = routingListRequest.stream()
                .map(RoutingsRequest::getId)
                .distinct()
                .toList();

        // Fetch routing from DB to ensure they exist before deletion
        List<Routings> routingToDelete = routingsDao.findByIdIn(routingIds);

        if (routingToDelete.isEmpty()) {
            throw new DataRetrievalFailureException("No routing found for the given Ids.");
        }

        // Validate that all necessary routing IDs are present in the request
        routingValidationUtil.validateUpdateBulkRequest(routingListRequest, routingToDelete);

        ParentResult parentResult = getParentDetails(routingToDelete, routingIds.get(0), module);

        // Delete routing from DB
        routingsDao.deleteByIdIn(routingIds);

        // Record audit logs for the deletion operation
        recordAuditLogs(routingToDelete, null, DBOperationType.DELETE, parentResult);

        // Return the response with status message
        return BulkRoutingResponse.builder()
                .message(prepareBulkDeleteMessage(routingToDelete))
                .build();
    }

    private String prepareBulkDeleteMessage(List<Routings> routings) {
        String message;

        // If more than one route was deleted, return a generic bulk success message
        if (routings.size() > 1) {
            message = "Routings deleted successfully!";
        } else {
            message = "Routing deleted successfully!";
        }

        return message;
    }

    @Override
    public Map<String, Object> getAllMasterData(CommonRequestModel commonRequestModel, String xSource) {
        Long id = commonRequestModel.getId();
        Optional<Routings> routings;
        if (Objects.equals(xSource, NETWORK_TRANSFER))
            routings = routingsDao.findByIdWithQuery(id);
        else
            routings = routingsDao.findById(id);
        if (routings.isEmpty()) {
            log.debug("Routing is null for the input id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        RoutingsResponse response = jsonHelper.convertValue(routings, RoutingsResponse.class);
        return getAllMasterDataForRoute(response);
    }

    @Override
    public List<Routings> getRoutingsByShipmentId(Long id) {
        return routingsDao.findByShipmentId(id);
    }

    @Override
    @Transactional
    public void deleteInheritedRoutingsFromShipment(List<ShipmentDetails> shipmentDetailsList) throws RunnerException {
        for (ShipmentDetails shipmentDetails : shipmentDetailsList) {
            List<Routings> routings = shipmentDetails.getRoutingsList();
            if (!CollectionUtils.isEmpty(routings)) {
                routings.removeIf(routing -> routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE && routing.getInheritedFromConsolidation() != null && routing.getInheritedFromConsolidation());
                BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
                bulkUpdateRoutingsRequest.setRoutings(jsonHelper.convertValueToList(routings, RoutingsRequest.class));
                bulkUpdateRoutingsRequest.setEntityId(shipmentDetails.getId());
                updateBulk(bulkUpdateRoutingsRequest, Constants.SHIPMENT);
            }
        }
    }

    @Override
    public List<Routings> getRoutingsByConsolidationId(Long consolidationId) {
        return routingsDao.findByConsolidationId(consolidationId);
    }

    @Override
    @Transactional
    public BulkRoutingResponse updateTransportInfoStatus(UpdateTransportStatusRequest request) throws RunnerException {
        if (TransportInfoStatus.IH.equals(request.getTransportInfoStatus())) {
            throw new ValidationException("Transport pol/pod info status can not be IH");
        }
        List<Routings> routingsList = new ArrayList<>();
        if(Constants.SHIPMENT.equalsIgnoreCase(request.getEntityType())) {
            Optional<ShipmentDetails> shipmentDetailsEntity = shipmentServiceV3.findById(request.getEntityId());
            if (shipmentDetailsEntity.isEmpty()) {
                throw new ValidationException("Invalid shipment id");
            }
           routingsList = shipmentDetailsEntity.get().getRoutingsList();
        } else if(Constants.CONSOLIDATION.equalsIgnoreCase(request.getEntityType())) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationV3Service.findById(request.getEntityId());
            if (consolidationDetails.isEmpty()) {
                throw new ValidationException("Invalid consolidation id");
            }
            routingsList = consolidationDetails.get().getRoutingsList();
        }
        routingsList.sort(Comparator.comparingLong(Routings::getLeg));
        List<RoutingsRequest> routingsRequests = jsonHelper.convertValue(routingsList, new TypeReference<List<RoutingsRequest>>() {
        });
        BulkUpdateRoutingsRequest bulkUpdateRoutingsRequest = new BulkUpdateRoutingsRequest();
        bulkUpdateRoutingsRequest.setTransportInfoStatus(request.getTransportInfoStatus());
        bulkUpdateRoutingsRequest.setRoutings(routingsRequests);
        bulkUpdateRoutingsRequest.setEntityId(request.getEntityId());
        return updateBulk(bulkUpdateRoutingsRequest, request.getEntityType());
    }

    public Map<String, Object> getAllMasterDataForRoute(RoutingsResponse response) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        try {
            double startTime = System.currentTimeMillis();
            var masterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllMasterDataInSingleCall(response, masterDataResponse)), executorServiceMasterData);
            var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllUnlocationInSingleCall(response, masterDataResponse)), executorServiceMasterData);
            var vesselDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> routingV3Util.addAllVesselInSingleCall(response, masterDataResponse)), executorServiceMasterData);
            CompletableFuture.allOf(locationDataFuture, masterDataFuture, vesselDataFuture).join();
            log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.ROUTING_RETRIEVE_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception ex) {
            log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_ROUTING_RETRIEVE, ex.getLocalizedMessage());
        }
        return masterDataResponse;
    }

    public List<Routings> reOrderRoutings(List<Routings> incomingRoutings, List<Routings> existingRoutings) {
        Set<UUID> existingGuids = existingRoutings.stream()
                .map(Routings::getGuid)
                .collect(Collectors.toSet());

        // First validate main carriage for inheritedFromConsolidation rules
        validateMainCarriage(incomingRoutings);

        // Reorder PRE and ON carriage to move violations to end of their blocks
        List<Routings> reorderedList = reorderPreAndOnCarriage(incomingRoutings);

        // Now separate based on carriage type after reordering
        List<Routings> preCarriage = new ArrayList<>();
        List<Routings> mainCarriage = new ArrayList<>();
        List<Routings> onCarriage = new ArrayList<>();

        for (Routings routing : reorderedList) {
            switch (routing.getCarriage().name()) {
                case "PRE_CARRIAGE" -> preCarriage.add(routing);
                case "MAIN_CARRIAGE" -> mainCarriage.add(routing);
                case "ON_CARRIAGE" -> onCarriage.add(routing);
                default -> throw new ValidationException("Invalid routing carriage");
            }
        }

        List<Routings> orderedPreCarriage = processCarriage(preCarriage, existingGuids);
        List<Routings> orderedMainCarriage = processMainCarriage(mainCarriage, existingGuids);
        List<Routings> orderedOnCarriage = processCarriage(onCarriage, existingGuids);

        AtomicLong legCount = new AtomicLong(1);
        List<Routings> finalOrderedRoutings = new ArrayList<>();
        mergeRoutingList(orderedPreCarriage, finalOrderedRoutings, legCount);
        mergeRoutingList(orderedMainCarriage, finalOrderedRoutings, legCount);
        mergeRoutingList(orderedOnCarriage, finalOrderedRoutings, legCount);

        return finalOrderedRoutings;
    }

    // ✅ Rule: No routing allowed between two inherited MAIN_CARRIAGE
    private void validateMainCarriage(List<Routings> routings) {
        boolean inInheritedBlock = false;
        int index = 0;
        int mainCarriageIndex = index;
        for (Routings routing : routings) {
            index++;
            if (routing.getCarriage() == RoutingCarriage.MAIN_CARRIAGE &&
                    Boolean.TRUE.equals(routing.getInheritedFromConsolidation())) {
                inInheritedBlock = true;
                mainCarriageIndex = index;
            } else if (inInheritedBlock) {
                // Found a non-inherited routing after inherited block started
                for (Routings remaining : routings.subList(mainCarriageIndex, routings.size())) {
                    if (remaining.getCarriage() == RoutingCarriage.MAIN_CARRIAGE &&
                            Boolean.TRUE.equals(remaining.getInheritedFromConsolidation())) {
                        throw new ValidationException("Invalid routing placement: inherited MAIN_CARRIAGE routing appears non-contiguously. Routing GUID: " + remaining.getGuid());
                    }
                }
                break;
            }
        }
    }

    // ✅ Rule: Move out-of-place routings in same-type blocks to the end
    private List<Routings> reorderPreAndOnCarriage(List<Routings> inputList) {
        List<Routings> result = new ArrayList<>();
        List<Routings> preBuffer = new ArrayList<>();
        List<Routings> onBuffer = new ArrayList<>();
        boolean inPreBlock = false;
        boolean inOnBlock = false;

        for (Routings routing : inputList) {
            RoutingCarriage type = routing.getCarriage();

            if (type == RoutingCarriage.PRE_CARRIAGE) {
                if (!inPreBlock) {
                    inPreBlock = true;
                    inOnBlock = false;
                }
                result.add(routing);
            } else if (type == RoutingCarriage.ON_CARRIAGE) {
                if (!inOnBlock) {
                    inOnBlock = true;
                    inPreBlock = false;
                }
                result.add(routing);
            } else {
                reOrderOtherCarriage(result, preBuffer, onBuffer, inPreBlock, inOnBlock, routing);
            }
        }

        // Append misplaced routings after their block
        int preInsertIndex = findLastIndexOfType(result, RoutingCarriage.PRE_CARRIAGE);
        result.addAll(preInsertIndex + 1, preBuffer);

        int onInsertIndex = findLastIndexOfType(result, RoutingCarriage.ON_CARRIAGE);
        result.addAll(onInsertIndex + 1, onBuffer);

        return result;
    }

    private static void reOrderOtherCarriage(List<Routings> result, List<Routings> preBuffer, List<Routings> onBuffer, boolean inPreBlock, boolean inOnBlock, Routings routing) {
        if (inPreBlock) {
            preBuffer.add(routing);
        } else if (inOnBlock) {
            onBuffer.add(routing);
        } else {
            result.add(routing);
        }
    }

    private int findLastIndexOfType(List<Routings> list, RoutingCarriage type) {
        for (int i = list.size() - 1; i >= 0; i--) {
            if (list.get(i).getCarriage() == type) return i;
        }
        return list.size() - 1;
    }

    // No change needed in these unless further optimization is desired
    private List<Routings> processMainCarriage(List<Routings> mainCarriage, Set<UUID> existingGuids) {
        List<Routings> result = new ArrayList<>();
        List<Routings> newToAppendAtEnd = new ArrayList<>();
        List<Routings> buffer = new ArrayList<>();
        boolean existingSeen = false;

        for (Routings current : mainCarriage) {
            boolean isExisting = existingGuids.contains(current.getGuid());

            if (isExisting) {
                if (!buffer.isEmpty()) {
                    int lastIndex = result.size() - 1;
                    boolean canInsert = true;

                    setMainCarriageInheritedIndexPosition(result, newToAppendAtEnd, buffer, current, lastIndex, canInsert);

                    buffer.clear();
                }

                result.add(current);
                existingSeen = true;
            } else {
                if (existingSeen) result.add(current);
                else buffer.add(current);
            }
        }

        result.addAll(buffer);
        result.addAll(newToAppendAtEnd);
        return result;
    }

    void setMainCarriageInheritedIndexPosition(List<Routings> result, List<Routings> newToAppendAtEnd, List<Routings> buffer, Routings current, int lastIndex, boolean canInsert) {
        if (lastIndex >= 0) {
            Routings prev = result.get(lastIndex);
            if (Boolean.TRUE.equals(prev.getInheritedFromConsolidation()) &&
                    Boolean.TRUE.equals(current.getInheritedFromConsolidation())) {
                canInsert = false;
            }
        }

        if (canInsert) result.addAll(buffer);
        else newToAppendAtEnd.addAll(buffer);
    }

    private List<Routings> processCarriage(List<Routings> carriage, Set<UUID> existingGuids) {
        List<Routings> result = new ArrayList<>();
        List<Routings> buffer = new ArrayList<>();
        boolean existingSeen = false;

        for (Routings current : carriage) {
            boolean isExisting = existingGuids.contains(current.getGuid());

            if (isExisting) {
                if (!buffer.isEmpty()) {
                    result.addAll(buffer);
                    buffer.clear();
                }
                result.add(current);
                existingSeen = true;
            } else {
                if (existingSeen) result.add(current);
                else buffer.add(current);
            }
        }

        result.addAll(buffer);
        return result;
    }

    private void mergeRoutingList(List<Routings> carriageRoute, List<Routings> routings, AtomicLong legCount) {
        if (carriageRoute.isEmpty()) return;

        carriageRoute.forEach(route -> {
            route.setLeg(legCount.get());
            legCount.incrementAndGet();
            routings.add(route);
        });
    }

    private Routings cloneRoutingForShipment(Routings source, Long shipmentId) {
        Routings cloned = new Routings();
        cloned.setShipmentId(shipmentId);
        cloned.setBookingId(null);
        cloned.setCarriage(source.getCarriage());
        cloned.setLeg(source.getLeg());
        cloned.setMode(source.getMode());
        cloned.setRoutingStatus(source.getRoutingStatus());
        cloned.setVesselName(source.getVesselName());
        cloned.setPol(source.getPol());
        cloned.setPod(source.getPod());
        cloned.setDomestic(source.getIsDomestic());
        cloned.setEta(source.getEta());
        cloned.setEtd(source.getEtd());
        cloned.setAta(source.getAta());
        cloned.setAtd(source.getAtd());
        // consolidationId should be null for the cloned routing being assigned to shipment
        cloned.setConsolidationId(null);
        cloned.setIsLinked(source.getIsLinked());
        cloned.setIsSelectedForDocument(source.getIsSelectedForDocument());
        cloned.setVoyage(source.getVoyage());
        cloned.setAircraftRegistration(source.getAircraftRegistration());
        cloned.setFlightNumber(source.getFlightNumber());
        cloned.setAircraftType(source.getAircraftType());
        cloned.setVehicleNumber(source.getVehicleNumber());
        cloned.setRouteLegId(source.getRouteLegId());
        cloned.setTransitDays(source.getTransitDays());
        cloned.setCarrier(source.getCarrier());
        cloned.setTruckReferenceNumber(source.getTruckReferenceNumber());
        cloned.setCarrierCountry(source.getCarrierCountry());
        cloned.setOriginPortLocCode(source.getOriginPortLocCode());
        cloned.setDestinationPortLocCode(source.getDestinationPortLocCode());
        cloned.setInheritedFromConsolidation(true); // Mark as inherited
        return cloned;
    }

    private boolean isInterBranchContextNeeded(ConsolidationDetails consolidationDetails) {

        if (Objects.isNull(consolidationDetails) || Objects.isNull(consolidationDetails.getShipmentsList()))
            return false;

        var interBranchShipments = consolidationDetails.getShipmentsList().stream()
                .filter(s -> !Objects.equals(s.getTenantId(), consolidationDetails.getTenantId())).findFirst();

        return interBranchShipments.isPresent() && Boolean.TRUE.equals(consolidationDetails.getInterBranchConsole());
    }
}
