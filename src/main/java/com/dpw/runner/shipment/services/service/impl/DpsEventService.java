package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.MatchingRulesRequest;
import com.dpw.runner.shipment.services.dto.response.DpsEventResponse;
import com.dpw.runner.shipment.services.entity.DpsEvent;
import com.dpw.runner.shipment.services.entity.DpsEvent.DpsFieldData;
import com.dpw.runner.shipment.services.entity.DpsEventLog;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.exception.exceptions.DpsException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import com.dpw.runner.shipment.services.service.handler.DpsWorkflowStateHandlerFactory;
import com.dpw.runner.shipment.services.service.handler.IDpsWorkflowStateHandler;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.google.common.base.Strings;

import java.util.*;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
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
    @Autowired
    private DpsWorkflowStateHandlerFactory dpsWorkflowStateHandlerFactory;

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
            ShipmentDetails shipmentDetails = shipmentDao.findByGuid(UUID.fromString(shipmentGuid))
                    .orElseThrow(() -> new DpsException("No Shipment found with GUID: " + shipmentGuid));

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
            DpsWorkflowState currentState = shipmentDetails.getDpsState();

            log.info("Handling DPS event for shipment GUID: {}, current state: {}, new state: {}",
                    dpsEvent.getEntityId(), currentState, newState);

            // Validate state transition
            IDpsWorkflowStateHandler handler = dpsWorkflowStateHandlerFactory.getHandler(currentState);
            handler.validateTransition(currentState, newState);

            // Update the state if validation succeeds
            shipmentDetails.setDpsState(newState);
            shipmentDao.save(shipmentDetails, false);
        } catch (Exception e) {
            throw new DpsException(e.getMessage(), e);
        }
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
    private void createAuditLog(DpsEvent dpsEvent, ShipmentDetails shipmentDetails) {
        try {
            DpsEventLog eventLog = DpsEventLog.builder()
                    .executionId(dpsEvent.getExecutionId().toString())
                    .usernameList(String.join(",", dpsEvent.getUsernameList()))
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

            // Update fields conditionally only if non-null
            if (dpsDto.getExecutionId() != null) {
                dpsEvent.setExecutionId(dpsDto.getExecutionId());
            }
            if (dpsDto.getEntityId() != null) {
                dpsEvent.setEntityId(dpsDto.getEntityId());
            }
            if (dpsDto.getWorkflowType() != null) {
                dpsEvent.setWorkflowType(DpsWorkflowType.valueOf(dpsDto.getWorkflowType()));
            }
            if (dpsDto.getState() != null) {
                dpsEvent.setState(DpsWorkflowState.valueOf(dpsDto.getState()));
            }
            if (dpsDto.getStatus() != null) {
                dpsEvent.setStatus(DpsExecutionStatus.valueOf(dpsDto.getStatus()));
            }
            if (dpsDto.getEntityType() != null) {
                dpsEvent.setEntityType(DpsEntityType.valueOf(dpsDto.getEntityType()));
            }
            if (dpsDto.getText() != null) {
                dpsEvent.setText(dpsDto.getText());
            }
            if (dpsDto.getImplications() != null) {
                dpsEvent.setImplicationList(dpsDto.getImplications());
            }
            if (dpsDto.getConditionMessage() != null) {
                dpsEvent.setConditionMessageList(dpsDto.getConditionMessage());
            }
            if (dpsDto.getUsernameList() != null) {
                dpsEvent.setUsernameList(dpsDto.getUsernameList());
            }
            if (dpsDto.getEventTimestamp() != null) {
                dpsEvent.setEventTimestamp(dpsDto.getEventTimestamp());
            }
            if (dpsDto.getFieldsDetected() != null || dpsDto.getFieldsDetectedValues() != null) {
                List<String> fieldsDetected = Optional.ofNullable(dpsDto.getFieldsDetected()).orElse(Collections.emptyList());
                List<String> fieldsDetectedValues = Optional.ofNullable(dpsDto.getFieldsDetectedValues()).orElse(Collections.emptyList());
                dpsEvent.setDpsFieldData(createDpsFieldDataList(fieldsDetected, fieldsDetectedValues));
            }

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

    /**
     * Updates the status of warning rules for a shipment based on the provided request.
     * This method is transactional, ensuring all database operations within it either succeed or fail as a unit.
     * It validates the input request, updates the rule statuses in the database, and returns an appropriate response.
     * If an exception occurs, it logs the error and returns a failure response.
     *
     * @param commonRequestModel the request model containing data for updating the warning rules status.
     *                           This includes details like the shipment GUID, username, and rule execution IDs.
     * @return a {@link ResponseEntity} containing an {@link IRunnerResponse}, indicating the success or failure of the operation.
     * @throws ValidationException if the input request is invalid (e.g., missing required fields).
     * @throws DpsException        if there are issues specific to the domain logic of updating the rules.
     */
    @Transactional
    public ResponseEntity<IRunnerResponse> updateWarningRulesStatus(CommonRequestModel commonRequestModel) {
        try {
            MatchingRulesRequest request = validateAndExtractRequest(commonRequestModel);
            dpsEventRepository.updateRuleStatus(
                    request.getUsername(),
                    DpsExecutionStatus.COMPLETED,
                    request.getRuleExecutionIds()
            );
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    /**
     * Validates and extracts the {@link MatchingRulesRequest} from the provided {@link CommonRequestModel}.
     * This method ensures the following:
     * - The request object is not null.
     * - The shipment GUID is present and not empty.
     * - The username is present and not empty.
     * - The rule execution IDs are provided and not empty.
     * If any of these validations fail, the method logs the error and throws a {@link ValidationException}.
     * @param commonRequestModel the request model containing data to be validated and extracted.
     * @return the validated {@link MatchingRulesRequest} object extracted from the input.
     * @throws ValidationException if the input request fails any of the validations.
     */
    private MatchingRulesRequest validateAndExtractRequest(CommonRequestModel commonRequestModel) {
        MatchingRulesRequest request = (MatchingRulesRequest) commonRequestModel.getData();
        if (request == null) {
            String errorMessage = ShipmentConstants.MATCHING_RULES_REQUEST_EMPTY_ERROR;
            log.error(errorMessage, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException(errorMessage);
        }
        if (Strings.isNullOrEmpty(request.getGuid())) {
            log.error(ShipmentConstants.REQUIRED_PARAMETER_NULL_OR_EMPTY_ERROR, ShipmentConstants.SHIPMENT_GUID, LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Shipment Guid is required but is null or empty.");
        }
        if (Strings.isNullOrEmpty(request.getUsername())) {
            log.error(ShipmentConstants.REQUIRED_PARAMETER_NULL_OR_EMPTY_ERROR, "Username", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Username is required but is null or empty.");
        }
        if (ObjectUtils.isEmpty(request.getRuleExecutionIds())) {
            log.error(ShipmentConstants.REQUIRED_PARAMETER_NULL_OR_EMPTY_ERROR, "Rule Execution List", LoggerHelper.getRequestIdFromMDC());
            throw new ValidationException("Rule Execution List is required but is null or empty.");
        }
        return request;
    }

}
