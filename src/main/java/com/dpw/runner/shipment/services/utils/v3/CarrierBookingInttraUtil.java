package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.InttraFailureException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.databind.JsonNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

@Slf4j
@Component
public class CarrierBookingInttraUtil {

    @Autowired
    MasterDataUtils masterDataUtils;

    @Autowired
    private ITransactionHistoryDao transactionHistoryDao;

    @Autowired
    BridgeServiceAdapter bridgeServiceAdapter;

    @Autowired
    MigrationUtil migrationUtil;

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    IConsolidationDetailsDao consolidationDetailsDao;

    public <T> BridgeServiceResponse sendPayloadToBridge(T inttraResponse, Long id,
                                                     String integrationCode, String transactionId, String referenceId,
                                                     IntegrationType integrationType, String entityType) throws RunnerException {
        String bridgePayload = jsonHelper.convertToJson(inttraResponse);
        log.info("Bridge payload {}", bridgePayload);
        BridgeServiceResponse bridgeServiceResponse;
        try {
            bridgeServiceResponse = (BridgeServiceResponse) bridgeServiceAdapter.bridgeApiIntegration(inttraResponse, integrationCode, transactionId, referenceId);
            migrationUtil.saveErrorResponse(id, entityType, integrationType, Status.SUCCESS, bridgeServiceResponse.toString());
            return bridgeServiceResponse;
        } catch (Exception exception) {
            log.error("Getting error from Bridge while uploading template to: {}", String.valueOf(exception));
            migrationUtil.saveErrorResponse(id, entityType, integrationType, Status.FAILED, exception.getMessage());
            throw new RunnerException("Getting error from Bridge");
        }
    }

    public void createTransactionHistory(String actionStatus, FlowType flowType, String description, SourceSystem sourceSystem, Long id, EntityTypeTransactionHistory entityType) {
        TransactionHistory transactionHistory = TransactionHistory.builder()
                .actionStatusDescription(actionStatus)
                .flowType(flowType)
                .description(description)
                .sourceSystem(sourceSystem)
                .actualDateTime(LocalDateTime.now())
                .entityType(entityType)
                .entityId(id)
                .build();
        transactionHistoryDao.save(transactionHistory);
    }

    public ConsolidationDetails getConsolidationDetail(Long id) {
        Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(id);
        if (consolidationDetails.isEmpty()) {
            throw new ValidationException("Consolidation details does not exist " + id);
        }
        return consolidationDetails.get();
    }

    public PartiesResponse fetchRequiredParty(Parties party) {
        if (Objects.isNull(party)) {
            return null;
        }

        return PartiesResponse.builder()
                .id(party.getId())
                .entityId(party.getEntityId())
                .entityType(party.getEntityType())
                .type(party.getType())
                .orgCode(party.getOrgCode())
                .tenantId(party.getTenantId())
                .addressCode(party.getAddressCode())
                .orgId(party.getOrgId())
                .addressId(party.getAddressId())
                .orgData(party.getOrgData())
                .addressData(party.getAddressData())
                .isAddressFreeText(party.getIsAddressFreeText())
                .countryCode(party.getCountryCode())
                .build();
    }

    public Map<String, EntityTransferCarrier> fetchCarrierDetailsForBridgePayload(SailingInformationResponse sailingInformation) {

        Map<String, EntityTransferCarrier> carrierDatav1Map = new HashMap<>();
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> carrierList = new HashSet<>();
            if (!Objects.isNull(sailingInformation)) {
                carrierList = new HashSet<>(masterDataUtils.createInBulkCarriersRequest(sailingInformation, SailingInformation.class, fieldNameKeyMap, SailingInformation.class.getSimpleName(), cacheMap));
            }
            if (CollectionUtils.isEmpty(carrierList)) {
                return new HashMap<>();
            }
            carrierDatav1Map = masterDataUtils.fetchInBulkCarriers(carrierList);
            return carrierDatav1Map;

        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCarrierDataInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
        return carrierDatav1Map;
    }

    public Map<String, EntityTransferContainerType> addAllContainerTypesInSingleCall(List<CommonContainerResponse> commonContainerResponses) {
        Map<String, EntityTransferContainerType> v1Data = new HashMap<>();
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            if (!Objects.isNull(commonContainerResponses)) {
                commonContainerResponses.forEach(r -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(r, CommonContainers.class, fieldNameKeyMap, CommonContainers.class.getSimpleName(), cacheMap)));
            }
            if (org.springframework.util.CollectionUtils.isEmpty(containerTypes)) {
                return new HashMap<>();
            }

            v1Data= masterDataUtils.fetchInBulkContainerTypes(containerTypes);
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllContainerTypesInSingleCall in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), MasterDataHelper.class.getSimpleName(), ex.getMessage());
        }
        return v1Data;
    }

    public String getInttraRemoteId(Parties[] allMadatoryParties) {
        // Check parties in order: requestor, shipper, forwardingAgent
        for (Parties party : allMadatoryParties) {
            if (party != null && party.getOrgData() != null &&
                    "INTRA_COMPANY_ID".equals(party.getOrgData().get("RemoteIdType"))) {
                return (String) party.getOrgData().get("RemoteId");
            } else if (party != null && party.getOrgData() != null &&
                    "INTRA_COMPANY_ID".equals(party.getOrgData().get("remoteIdType"))) {
                return (String) party.getOrgData().get("remoteId");
            }
        }
        return null;
    }


    public Map<String, EntityTransferUnLocations> fetchUnLocationMap(CarrierBooking carrierBooking ){
        SailingInformation sailingInformationResponse = carrierBooking.getSailingInformation();
        Set<String> locationCodes = Stream.of(
                        sailingInformationResponse.getPod(),
                        sailingInformationResponse.getPol(),
                        sailingInformationResponse.getCarrierDeliveryPlace(),
                        sailingInformationResponse.getCarrierReceiptPlace(),
                        carrierBooking.getBookingOffice()
                )
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<String, EntityTransferUnLocations> locationsMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        return locationsMap;
    }

    public void validateContainersIntegrationCode(List<CommonContainerResponse> containersList) {

        if (containersList == null || containersList.isEmpty()) {
            return;
        }
        String invalidContainers = containersList.stream()
                .filter(c -> c.getIntegrationCode() == null || c.getIntegrationCode().trim().isEmpty())
                .map(c -> c.getContainerNo() != null ? c.getContainerNo() : "Container ID: " + c.getId())
                .collect(Collectors.joining(", "));

        if (!invalidContainers.isEmpty()) {
            throw new ValidationException(
                    "IntegrationCode is a mandatory field for INTTRA. Missing for containers: " + invalidContainers
            );
        }
    }
}
