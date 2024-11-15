package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
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
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import com.google.common.base.Strings;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import javax.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
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
    @Override
    public List<DpsEvent> getMatchingRulesByGuid(CommonRequestModel commonRequestModel) {

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

    private DpsEvent constructDpsEvent(DpsDto dpsDto) {
        try {

            return new DpsEvent()
                    .setExecutionId(dpsDto.getExecutionId())
                    .setEntityId(dpsDto.getEntityId())
                    .setWorkflowType(DpsWorkflowType.valueOf(dpsDto.getWorkflowType()))
                    .setState(DpsWorkflowState.valueOf(dpsDto.getState()))
                    .setStatus(DpsExecutionStatus.valueOf(dpsDto.getStatus()))
                    .setEntityType(DpsEntityType.valueOf(dpsDto.getEntityType()))
                    .setText(dpsDto.getText())
                    .setImplicationList(dpsDto.getImplications())
                    .setConditionMessageList(dpsDto.getConditionMessage())
                    .setDpsFieldData(createDpsFieldDataList(
                            dpsDto.getFieldsDetected(), dpsDto.getFieldsDetectedValues()));

        } catch (Exception e) {
            throw new DpsException("Error in creating object of DpsEvent: " + e.getMessage(), e);
        }
    }

    private List<DpsFieldData> createDpsFieldDataList(List<String> keys, List<String> values) {
        List<DpsFieldData> dpsFieldDataList = new ArrayList<>();

        for (int i = 0; i < keys.size(); i++) {
            dpsFieldDataList.add(new DpsFieldData(keys.get(i), values.get(i)));
        }

        return dpsFieldDataList;
    }


}
