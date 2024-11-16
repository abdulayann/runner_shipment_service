package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.DpsEventResponse;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.DpsEvent.DpsFieldData;
import com.dpw.runner.shipment.services.entity.DpsEventLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.google.common.base.Strings;

import java.util.*;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class DpsEventService implements IDpsEventService {

    @Autowired
    private IDpsEventRepository dpsEventRepository;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private IShipmentDao shipmentDao;

    @Override
    @Transactional
    public DpsEvent saveDpsEvent(DpsDto dpsDto){
        DpsEvent dpsEvent = constructDpsEvent(dpsDto);
        createAuditLog(dpsEvent);
        return dpsEventRepository.save(dpsEvent);
    }

    /**
     * Creates an audit log entry for a DPS event if the event is related to a shipment.
     * This method checks the entity type of the event, retrieves shipment details based on the GUID,
     * and logs audit information to track changes or activities related to the shipment.
     *
     * @param dpsEvent the DPS event containing information about an action taken on a shipment
     * @throws DpsException if the {@code dpsEvent} has a null or empty GUID,
     *                      if no shipment is found for the given GUID,
     *                      or if an unexpected error occurs during audit log creation
     */
    private void createAuditLog(DpsEvent dpsEvent) {
        try {
            Objects.requireNonNull(dpsEvent, "DpsEvent cannot be null");

            // Check if the event type is 'SHIPMENT' before proceeding
            if (DpsEntityType.SHIPMENT.equals(dpsEvent.getEntityType())) {
                String shipmentGuid = dpsEvent.getEntityId();

                if (shipmentGuid == null || shipmentGuid.isBlank()) {
                    throw new DpsException("Shipment GUID is null or empty in DPS event");
                }

                // Retrieve shipment details using the GUID, throwing an exception if not found
                ShipmentDetails shipmentDetails = shipmentDao.findByGuid(UUID.fromString(shipmentGuid))
                        .orElseThrow(() -> new DpsException("No Shipment found with GUID: " + shipmentGuid));

                // Construct a DpsEventLog object with relevant information for the audit log
                DpsEventLog eventLog = DpsEventLog.builder()
                        .executionId(dpsEvent.getExecutionId().toString())
                        .usernameList(String.join(",", dpsEvent.getUsernameList()))
                        .dpsWorkflowState(dpsEvent.getState())
                        .eventTimeStamp(dpsEvent.getEventTimestamp())
                        .shipmentId(shipmentDetails.getShipmentId())
                        .tenantId(shipmentDetails.getTenantId())
                        .build();

                // Build the audit log metadata with relevant details
                AuditLogMetaData auditLogMetaData = AuditLogMetaData.builder()
                        .tenantId(shipmentDetails.getTenantId())
                        .newData(eventLog)
                        .parent(ShipmentDetails.class.getSimpleName())
                        .parentId(shipmentDetails.getId())
                        .entityType(DpsEventLog.class.getSimpleName())
                        .operation(DBOperationType.LOG.name())
                        .build();

                // Add the constructed audit log metadata to the audit log service
                auditLogService.addAuditLog(auditLogMetaData);

                log.info("Audit log created successfully for shipment GUID: {}", shipmentGuid);
            }
        } catch (Exception e) {
            throw new DpsException("DPS EVENT LOG ERROR -- " + e.getMessage(), e);
        }
    }

    /**
     * Retrieves a list of active DPS events that match a given GUID from the specified request model.
     * The method extracts the GUID from the {@code commonRequestModel}, validates it,
     * and fetches active {@link DpsEvent} entries associated with the GUID.
     *
     * @param commonRequestModel the request model containing the GUID used to filter DPS events
     * @return a list of active {@link DpsEvent} entries matching the given GUID
     * @throws DpsException if the GUID is null or empty, or if an error occurs during data retrieval
     */
    private List<DpsEvent> fetchMatchingRulesByGuid(CommonRequestModel commonRequestModel) {

        try {
            CommonGetRequest commonGetRequest = (CommonGetRequest) commonRequestModel.getData();
            String guid = commonGetRequest.getGuid();
            if (Strings.isNullOrEmpty(guid)) {
                log.error("GUID is null for DpsEvent retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                throw new DpsException("GUID can't be null. Please provide guid!");
            }
            return dpsEventRepository.findDpsEventByGuidAndExecutionState(guid, DpsExecutionStatus.ACTIVE);
        } catch (Exception e) {
            throw new DpsException("Error in fetching object of DpsEvent: " + e.getMessage(), e);
        }
    }

    /**
     * Retrieves matching rules based on a unique identifier provided in the {@code CommonRequestModel}.
     * The method filters and organizes rules into categories by workflow type (HOLD or WARNING),
     * constructing an appropriate response for each.
     *
     * @param commonRequestModel the model containing the unique identifier and necessary request details
     * @return {@code ResponseEntity<IRunnerResponse>} containing the mapped matching rules if successful;
     *         otherwise, an error response entity
     */
    @Override
    public ResponseEntity<IRunnerResponse> getShipmentMatchingRulesByGuid(CommonRequestModel commonRequestModel) {
        try {
            List<DpsEvent> dpsEvents = Optional.ofNullable(fetchMatchingRulesByGuid(commonRequestModel)).orElseGet(ArrayList::new);

            Map<DpsWorkflowType, List<DpsEventResponse>> responseMap = new HashMap<>();
            responseMap.put(DpsWorkflowType.HOLD, new ArrayList<>());
            responseMap.put(DpsWorkflowType.WARNING, new ArrayList<>());

            for (DpsEvent dpsEvent : dpsEvents) {
                if (dpsEvent == null) continue;

                DpsWorkflowType workflowType = dpsEvent.getWorkflowType();
                if (dpsEvent.getEntityType() == DpsEntityType.SHIPMENT && (workflowType == DpsWorkflowType.HOLD || workflowType == DpsWorkflowType.WARNING)) {
                    responseMap.computeIfAbsent(workflowType, k -> new ArrayList<>()).add(constructDpsEventResponse(dpsEvent));
                }
            }
            return ResponseHelper.buildSuccessResponse(responseMap);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    /**
     * Constructs a {@link DpsEventResponse} object from a {@link DpsEvent} instance. This method
     * maps the fields in the input {@code DpsEvent} to a new {@code DpsEventResponse} object,
     * including transformation of field data if present.
     *
     * @param dpsEvent the {@code DpsEvent} object containing data to be transformed into a response
     * @return a {@code DpsEventResponse} containing the mapped fields from the input {@code DpsEvent}
     * @throws DpsException if an error occurs during response construction
     */
    private DpsEventResponse constructDpsEventResponse(DpsEvent dpsEvent) {
        try {
            List<DpsEventResponse.DpsFieldDataResponse> dpsFieldDataResponseList =
                    dpsEvent.getDpsFieldData() != null
                            ? dpsEvent.getDpsFieldData().stream()
                            .map(dpsFieldData -> new DpsEventResponse.DpsFieldDataResponse(dpsFieldData.getKey(), dpsFieldData.getValue()))
                            .collect(Collectors.toList())
                            : Collections.emptyList();

            return  DpsEventResponse.builder()
                    .id(dpsEvent.getId())
                    .guid(dpsEvent.getGuid())
                    .executionId(dpsEvent.getExecutionId())
                    .entityId(dpsEvent.getEntityId())
                    .entityType(dpsEvent.getEntityType())
                    .workflowType(dpsEvent.getWorkflowType())
                    .state(dpsEvent.getState())
                    .status(dpsEvent.getStatus())
                    .text(dpsEvent.getText())
                    .implicationList(dpsEvent.getImplicationList() != null ? new ArrayList<>(dpsEvent.getImplicationList()) : new ArrayList<>())
                    .conditionMessageList(dpsEvent.getConditionMessageList() != null ? new ArrayList<>(dpsEvent.getConditionMessageList()) : new ArrayList<>())
                    .dpsFieldData(dpsFieldDataResponseList)
                    .usernameList(dpsEvent.getUsernameList())
                    .eventTimestamp(dpsEvent.getEventTimestamp())
                    .build();
        } catch (Exception e) {
            throw new DpsException("Error while constructing DpsEventResponse: " + e.getMessage(), e);
        }
    }

    /**
     * Constructs or updates a {@link DpsEvent} instance based on the provided {@link DpsDto}.
     * If an existing {@link DpsEvent} is found by the execution ID, it updates its fields.
     * Otherwise, it creates a new {@link DpsEvent} and sets all the required fields.
     *
     * @param dpsDto the {@link DpsDto} object containing data for creating or updating a {@link DpsEvent}.
     * @return a fully constructed or updated {@link DpsEvent} object.
     * @throws DpsException if an error occurs during the creation or update process.
     */
    private DpsEvent constructDpsEvent(DpsDto dpsDto) {
        try {
            // Attempt to find an existing DpsEvent by execution ID, or create a new one if not found
            DpsEvent dpsEvent = Optional.ofNullable(dpsEventRepository.findByExecutionId(dpsDto.getExecutionId()))
                    .orElseGet(DpsEvent::new);

            // Set or update all fields except for the execution ID
            dpsEvent.setExecutionId(dpsDto.getExecutionId()) // Retain the existing execution ID
                    .setEntityId(dpsDto.getEntityId())
                    .setWorkflowType(DpsWorkflowType.valueOf(dpsDto.getWorkflowType()))
                    .setState(DpsWorkflowState.valueOf(dpsDto.getState()))
                    .setStatus(DpsExecutionStatus.valueOf(dpsDto.getStatus()))
                    .setEntityType(DpsEntityType.valueOf(dpsDto.getEntityType()))
                    .setText(dpsDto.getText())
                    .setImplicationList(dpsDto.getImplications())
                    .setConditionMessageList(dpsDto.getConditionMessage())
                    .setUsernameList(dpsDto.getUsernameList())
                    .setEventTimestamp(dpsDto.getEventTimestamp())
                    .setDpsFieldData(createDpsFieldDataList(
                            dpsDto.getFieldsDetected(), dpsDto.getFieldsDetectedValues()));

            return dpsEvent;

        } catch (Exception e) {
            throw new DpsException("Error in creating or updating object of DpsEvent: " + e.getMessage(), e);
        }
    }

    /**
     * Creates a list of {@link DpsFieldData} objects by mapping the provided keys to their corresponding values.
     *
     * @param keys the list of field keys.
     * @param values the list of field values corresponding to the keys.
     * @return a list of {@link DpsFieldData} objects constructed from the provided keys and values.
     * @throws IndexOutOfBoundsException if the size of the keys and values lists do not match.
     */
    private List<DpsFieldData> createDpsFieldDataList(List<String> keys, List<String> values) {
        List<DpsFieldData> dpsFieldDataList = new ArrayList<>();

        for (int i = 0; i < keys.size(); i++) {
            dpsFieldDataList.add(new DpsFieldData(keys.get(i), values.get(i)));
        }

        return dpsFieldDataList;
    }


}
