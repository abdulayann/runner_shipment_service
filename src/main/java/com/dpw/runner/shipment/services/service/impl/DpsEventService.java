package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
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
import com.dpw.runner.shipment.services.kafka.dto.DpsDto;
import com.dpw.runner.shipment.services.repository.interfaces.IDpsEventRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IDpsEventService;
import java.util.ArrayList;
import java.util.List;
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

    /**
     * Saves a DPS event after constructing it from the provided DTO and creates an associated audit log.
     *
     * @param dpsDto Data transfer object containing DPS event details.
     * @return The saved {@link DpsEvent} entity.
     */
    @Override
    @Transactional
    public DpsEvent saveDpsEvent(DpsDto dpsDto) {
        log.info("Starting saveDpsEvent process for executionId: {}", dpsDto.getExecutionId());

        // Construct the DPS event object from the provided DTO
        DpsEvent dpsEvent = constructDpsEvent(dpsDto);

        // Create an audit log entry for the DPS event
        createAuditLog(dpsEvent);

        // Save and return the DPS event entity
        DpsEvent savedEvent = dpsEventRepository.save(dpsEvent);
        log.info("DPS Event successfully saved with ID: {}", savedEvent.getId());
        return savedEvent;
    }

    /**
     * Creates an audit log for a given DPS event.
     *
     * @param dpsEvent The DPS event for which the audit log is created.
     */
    private void createAuditLog(DpsEvent dpsEvent) {
        try {
            log.info("Creating audit log for DPS event with entityId: {}", dpsEvent.getEntityId());

            // Fetch associated ShipmentDetails based on the DPS event's entity ID
            ShipmentDetails shipmentDetails = shipmentDao.findByGuid(UUID.fromString(dpsEvent.getEntityId()))
                    .orElseThrow(() -> new DpsException("Shipment not found for UUID: " + dpsEvent.getEntityId()));

            // Build and add the audit log entry
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(shipmentDetails.getTenantId())
                            .userName(dpsEvent.getUsername())
                            .newData(DpsEventLog.builder()
                                    .eventTimestamp(dpsEvent.getEventTimestamp())
                                    .type(dpsEvent.getWorkflowType())
                                    .username(dpsEvent.getUsername()).build())
                            .prevData(null)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentDetails.getId())
                            .entityType(DpsEventLog.class.getSimpleName())
                            .operation(DBOperationType.LOG.name()).build()
            );

            log.info("Audit log created successfully for DPS event with entityId: {}", dpsEvent.getEntityId());
        } catch (Exception e) {
            log.error("DPS ERROR -- Error while creating audit log for DPS Event. {}", e.getMessage());
            throw new DpsException("DPS ERROR -- Error while creating audit log for DPS Event. " + e.getMessage(), e);
        }
    }

    /**
     * Constructs a {@link DpsEvent} entity from the given DTO.
     *
     * @param dpsDto DTO containing the details needed to construct a DPS event.
     * @return A fully constructed {@link DpsEvent} entity.
     */
    private DpsEvent constructDpsEvent(DpsDto dpsDto) {
        try {
            log.debug("Constructing DpsEvent for executionId: {}", dpsDto.getExecutionId());

            // Set attributes from the DTO to the DPS event entity
            return new DpsEvent()
                    .setExecutionId(dpsDto.getExecutionId())
                    .setEntityId(dpsDto.getEntityId())
                    .setWorkflowType(DpsWorkflowType.valueOf(dpsDto.getWorkflowType()))
                    .setState(DpsWorkflowState.valueOf(dpsDto.getState()))
                    .setStatus(DpsExecutionStatus.valueOf(dpsDto.getStatus()))
                    .setEntityType(DpsEntityType.valueOf(dpsDto.getEntityType()))
                    .setText(dpsDto.getText())
                    .setUsername(dpsDto.getUsername())
                    .setEventTimestamp(dpsDto.getEventTimestamp())
                    .setImplicationList(dpsDto.getImplications())
                    .setConditionMessageList(dpsDto.getConditionMessage())
                    .setDpsFieldData(createDpsFieldDataList(dpsDto.getFieldsDetected(), dpsDto.getFieldsDetectedValues()));

        } catch (Exception e) {
            log.error("Error while constructing DpsEvent: {}", e.getMessage());
            throw new DpsException("Error in creating object of DpsEvent: " + e.getMessage(), e);
        }
    }

    /**
     * Creates a list of {@link DpsFieldData} from provided keys and values.
     *
     * @param keys   List of field names (keys) detected.
     * @param values List of field values corresponding to each key.
     * @return List of {@link DpsFieldData} objects.
     */
    private List<DpsFieldData> createDpsFieldDataList(List<String> keys, List<String> values) {
        log.debug("Creating DpsFieldData list from keys and values.");
        List<DpsFieldData> dpsFieldDataList = new ArrayList<>();

        // Map each key to its corresponding value and add to the DPS field data list
        for (int i = 0; i < keys.size(); i++) {
            dpsFieldDataList.add(new DpsFieldData(keys.get(i), values.get(i)));
            log.trace("Added DpsFieldData with key: {} and value: {}", keys.get(i), values.get(i));
        }

        log.debug("DpsFieldData list created with {} items.", dpsFieldDataList.size());
        return dpsFieldDataList;
    }


}
