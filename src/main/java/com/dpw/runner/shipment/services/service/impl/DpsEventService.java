package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
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
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto.DpsDataDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.google.common.base.Strings;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
@Slf4j
public class DpsEventService implements IDpsEventService {

    @Autowired
    private IDpsEventRepository dpsEventRepository;
    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private IShipmentDao shipmentDao;

    /**
     * Saves a new or updates an existing {@link DpsEvent} based on the provided {@link DpsDto}. This method constructs a {@link DpsEvent} from the given DTO, saves it to the
     * repository, and processes the saved event accordingly.
     * <p>
     * The method is annotated with {@link Transactional} to ensure that the operation is performed within a transaction context, guaranteeing consistency and rollback if any
     * exception occurs during the saving or processing steps.
     *
     * @param dpsDto the data transfer object containing the details to create or update a {@link DpsEvent}
     * @return the saved {@link DpsEvent} object after persisting it to the database
     * @throws DpsException if any error occurs during the process, such as failure in constructing the {@link DpsEvent}, saving it to the repository, or processing the event
     */
    @Override
    @Transactional
    public DpsEvent saveDpsEvent(DpsDto dpsDto) {
        DpsEvent dpsEvent = constructDpsEvent(dpsDto);
        DpsEvent savedEvent = dpsEventRepository.save(dpsEvent);
        processDpsEvent(savedEvent);
        return savedEvent;
    }

    /**
     * Processes the provided {@link DpsEvent} based on the entity type.
     * If the entity type is {@link DpsEntityType#SHIPMENT}, it retrieves the associated
     * shipment details by GUID and handles the event, followed by creating an audit log.
     *
     * <p>Logs important information and throws {@link DpsException} for missing or invalid data.
     * Logs include the GUID of the shipment and confirmation messages for event handling and audit logging.</p>
     *
     * @param dpsEvent the {@link DpsEvent} to be processed
     * @throws DpsException if the entity type is {@link DpsEntityType#SHIPMENT} and the shipment GUID
     *                      is null, empty, or if no matching shipment is found in the database.
     */
    private void processDpsEvent(DpsEvent dpsEvent) {
        if (DpsEntityType.SHIPMENT.equals(dpsEvent.getEntityType())) {
            String shipmentGuid = dpsEvent.getEntityId();

            if (shipmentGuid == null || shipmentGuid.isBlank()) {
                throw new DpsException("Shipment GUID is null or empty in DPS event");
            }

            // Fetch ShipmentDetails once
            ShipmentDetails shipmentDetails = shipmentDao.findShipmentsByGuids(Set.of(UUID.fromString(shipmentGuid)))
                    .stream().findFirst().orElseThrow(() -> new DpsException("No Shipment found with GUID: " + shipmentGuid));

            // Handle DPS events
            handleDpsEvents(dpsEvent, shipmentDetails);

            // Create audit log
            createAuditLog(dpsEvent, shipmentDetails);
        }
    }
    /**
     * Handles DPS event transitions for a given {@link DpsEvent} and {@link ShipmentDetails}. This method validates the state transition according to the current state and the new
     * state from the DPS event. If the transition is valid, it updates the state of the shipment and persists the changes. If an error occurs during processing, a
     * {@link DpsException} is thrown.
     *
     * <p>Logs important details for tracing and validation, including current and new states and any exceptions encountered.</p>
     *
     * @param dpsEvent        the {@link DpsEvent} to be processed
     * @param shipmentDetails the {@link ShipmentDetails} associated with the shipment being handled
     * @throws DpsException if the state transition validation fails or if there is an error while updating the shipment state
     */
    private void handleDpsEvents(DpsEvent dpsEvent, ShipmentDetails shipmentDetails) {
        try {
            DpsWorkflowState newState = dpsEvent.getState();

            log.info("Saving DPS event for shipment GUID: {}, state: {}",
                    dpsEvent.getEntityId(), newState);

            // Update the state if validation succeeds
            shipmentDao.saveDpsState(shipmentDetails.getId(), newState.name());
        } catch (Exception e) {
            throw new DpsException(e.getMessage(), e);
        }
    }

    /**
     * Retrieves the list of implications associated with a given entity ID
     * for the entity type "SHIPMENT" and status "ACTIVE".
     *
     * @param shipmentGuid the GUID of the entity for which implications are to be fetched.
     *                 Must not be null or empty.
     * @return a list of implications corresponding to the provided entity ID.
     * @throws DpsException if the entity ID is null, empty, or no implications are found
     *                      for the specified entity ID.
     */
    @Override
    public List<String> getImplicationsForShipment(String shipmentGuid) {
        if (Strings.isNullOrEmpty(shipmentGuid)) {
            throw new DpsException("Shipment guid cannot be null or empty!");
        }

        List<String> implications = dpsEventRepository.findImplicationsByEntityIdAndEntityType(
                shipmentGuid,
                DpsEntityType.SHIPMENT.name(),
                DpsExecutionStatus.ACTIVE.name()
        );

        if (CollectionUtils.isEmpty(implications)) {
            log.warn("No implications found for the provided entity ID: {}" , shipmentGuid);
            return Collections.emptyList();
        }

        return implications;
    }
    /**
     * Creates an audit log entry for the given {@link DpsEvent} and {@link ShipmentDetails}. This method captures relevant details about the DPS event and the associated shipment,
     * and persists the audit information for tracking and future reference.
     *
     * <p>The audit log typically includes the shipment GUID, the event details, and any
     * other relevant metadata to help with tracking the changes made to the shipment's state.</p>
     *
     * @param dpsEvent        the {@link DpsEvent} that triggered the audit log creation
     * @param shipmentDetails the {@link ShipmentDetails} associated with the shipment being audited
     * @throws DpsException if there is an error while creating the audit log or saving it
     */
    @Override
    public void createAuditLog(DpsEvent dpsEvent, ShipmentDetails shipmentDetails) {
        try {
            DpsEventLog eventLog = DpsEventLog.builder()
                    .executionId(dpsEvent.getExecutionId().toString())
                    .transactionId(dpsEvent.getTransactionId())
                    .usernameList(ObjectUtils.isNotEmpty(dpsEvent.getUsernameList()) ? String.join(",", dpsEvent.getUsernameList()) : null)
                    .dpsWorkflowState(dpsEvent.getState())
                    .eventTimeStamp(dpsEvent.getEventTimestamp())
                    .shipmentId(shipmentDetails.getShipmentId())
                    .tenantId(shipmentDetails.getTenantId())
                    .build();

            AuditLogMetaData auditLogMetaData = AuditLogMetaData.builder()
                    .tenantId(shipmentDetails.getTenantId())
                    .newData(eventLog)
                    .parent(ShipmentDetails.class.getSimpleName())
                    .parentId(shipmentDetails.getId())
                    .entityType(DpsEventLog.class.getSimpleName())
                    .operation(DBOperationType.LOG.name())
                    .build();

            auditLogService.addAuditLog(auditLogMetaData);

            log.info("Audit log created successfully for shipment GUID: {}", dpsEvent.getEntityId());
        } catch (Exception e) {
            throw new DpsException("DPS EVENT LOG ERROR -- " + e.getMessage(), e);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getShipmentMatchingRulesByGuid(String shipmentGuid) {

        if (Strings.isNullOrEmpty(shipmentGuid)) {
            throw new DpsException("GUID can't be null. Please provide guid!");
        }
        List<DpsEvent> dpsEventList = dpsEventRepository.findDpsEventByGuidAndExecutionState(shipmentGuid, DpsExecutionStatus.ACTIVE.name());

        if(ObjectUtils.isEmpty(dpsEventList)) {
            log.warn("No DPS Event found with provided entity id {}", shipmentGuid);
            return ResponseHelper.buildSuccessResponse(Collections.emptyList());
        } else {
            List<DpsEventResponse> dpsEventResponses = dpsEventList.stream()
                    .map(this::constructDpsEventResponse).toList();
            return ResponseHelper.buildSuccessResponse(dpsEventResponses);
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
    @Override
    public DpsEventResponse constructDpsEventResponse(DpsEvent dpsEvent) {
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
                    .matchingCondition(dpsEvent.getMatchingCondition())
                    .implicationList(dpsEvent.getImplicationList() != null ? new ArrayList<>(dpsEvent.getImplicationList()) : new ArrayList<>())
                    .conditionMessageList(dpsEvent.getConditionMessageList() != null ? new ArrayList<>(dpsEvent.getConditionMessageList()) : new ArrayList<>())
                    .dpsFieldData(dpsFieldDataResponseList)
                    .usernameList(dpsEvent.getUsernameList() != null ? new ArrayList<>(dpsEvent.getUsernameList()) : new ArrayList<>())
                    .eventTimestamp(dpsEvent.getEventTimestamp())
                    .tasks(dpsEvent.getTasks() != null ? new ArrayList<>(dpsEvent.getTasks()) : new ArrayList<>()).build();
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
    @Override
    public DpsEvent constructDpsEvent(DpsDto dpsDto) {
        try {
            // Attempt to find an existing DpsEvent by execution ID, or create a new one if not found
            DpsDataDto dtoData = dpsDto.getData();
            if (dtoData == null) {
                throw new DpsException("DpsDataDto cannot be null.");
            }

            // Fetch existing DpsEvent or create a new one
            DpsEvent dpsEvent = dpsEventRepository.findByExecutionId(dtoData.getRuleExecutionId());
            if (dpsEvent == null) {
                // Validate mandatory fields for creating a new DpsEvent
                if (dtoData.getEntityType() == null || dtoData.getEntityId() == null) {
                    throw new DpsException("Entity Type and Entity ID are mandatory for new Dps rule execution id: "
                            + dtoData.getRuleExecutionId());
                }
                dpsEvent = new DpsEvent();
            }

            // Update fields conditionally only if non-null
            if (dtoData.getRuleExecutionId() != null) {
                dpsEvent.setExecutionId(dtoData.getRuleExecutionId());
            }
            if (dtoData.getEntityId() != null) {
                dpsEvent.setEntityId(dtoData.getEntityId());
            }
            if (dtoData.getWorkflowType() != null) {
                dpsEvent.setWorkflowType(DpsWorkflowType.valueOf(dtoData.getWorkflowType()));
            }
            if (dtoData.getState() != null) {
                dpsEvent.setState(DpsWorkflowState.valueOf(dtoData.getState()));
            }
            if (dtoData.getRuleStatus() != null) {
                dpsEvent.setStatus(DpsExecutionStatus.valueOf(dtoData.getRuleStatus()));
            }
            if (dtoData.getEntityType() != null) {
                dpsEvent.setEntityType(DpsEntityType.valueOf(dtoData.getEntityType()));
            }
            if (dtoData.getText() != null) {
                dpsEvent.setText(dtoData.getText());
            }
            if (dtoData.getMatchingCondition() != null) {
                dpsEvent.setMatchingCondition(dtoData.getMatchingCondition());
            }
            if (dtoData.getImplications() != null) {
                dpsEvent.setImplicationList(dtoData.getImplications());
            }
            if (dtoData.getConditionMessage() != null) {
                dpsEvent.setConditionMessageList(dtoData.getConditionMessage());
            }
            if (dtoData.getRuleMatchedFieldList() != null) {
                dpsEvent.setRuleMatchedFieldList(dtoData.getRuleMatchedFieldList());
            }
            if (dtoData.getUsernameList() != null) {
                dpsEvent.setUsernameList(dtoData.getUsernameList());
            }
            if (dtoData.getTasks() != null) {
                dpsEvent.setTasks(dtoData.getTasks());
            }
            if (dtoData.getEventTimestamp() != null) {
                dpsEvent.setEventTimestamp(dtoData.getEventTimestamp());
            }
            if (dpsDto.getTransactionId() != null) {
                dpsEvent.setTransactionId(dpsDto.getTransactionId());
            }
            if (dtoData.getFieldsDetectedValues() != null) {
                List<DpsFieldData> fieldDataList = ObjectUtils.defaultIfNull(dpsEvent.getDpsFieldData(), new ArrayList<>());
                dtoData.getFieldsDetectedValues().forEach(fieldDataDto ->
                        fieldDataList.add(DpsFieldData.builder()
                                .key(fieldDataDto.getKey())
                                .value(fieldDataDto.getValue())
                                .build())
                );
                dpsEvent.setDpsFieldData(fieldDataList); // Set the updated list back to the entity
            }

            return dpsEvent;

        } catch (Exception e) {
            throw new DpsException("Error in creating or updating object of DpsEvent: " + e.getMessage(), e);
        }
    }


}
