package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.utils.ContainerValidationUtil;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class ContainerV3Service implements IContainerV3Service {

    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ContainerValidationUtil containerValidationUtil;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private KafkaProducer producer;

    @Value("${containersKafka.queue}")
    private String senderQueue;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private IContainersSync containersSync;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private ExecutorService executorService;

    @Override
    public ContainerResponse create(ContainerRequest containerRequest) {
        Containers container = jsonHelper.convertValue(containerRequest, Containers.class);
        Containers savedContainer = containerRepository.save(container);
        return jsonHelper.convertValue(savedContainer, ContainerResponse.class);
    }

    @Override
    @Transactional
    public BulkContainerResponse updateBulk(List<ContainerRequest> containerRequestList) {
        // Validate the incoming request to ensure all mandatory fields are present
        containerValidationUtil.validateUpdateBulkRequest(containerRequestList);

        // Convert the request DTOs to entity models for persistence
        List<Containers> originalContainers = jsonHelper.convertValueToList(containerRequestList, Containers.class);

        // Save the updated containers to the database
        List<Containers> updatedContainers = containerDao.saveAll(originalContainers);

        // Execute post-save processing for each container asynchronously
        CompletableFuture<Void> afterSaveFuture = CompletableFuture.allOf(
                updatedContainers.stream()
                        .map(container -> CompletableFuture.runAsync(masterDataUtils.withMdc(() ->
                                        afterSave(container, false)),
                                executorService)
                        ).toArray(CompletableFuture[]::new)
        );

        // Run container sync operations in parallel for each updated container
        CompletableFuture<Void> containerSyncFuture = CompletableFuture.allOf(
                updatedContainers.stream()
                        .map(container -> CompletableFuture.runAsync(masterDataUtils.withMdc(() -> {
                                            Long containerId = container.getId();
                                            List<ShipmentsContainersMapping> mappings = shipmentsContainersMappingDao.findByContainerIdIn(List.of(containerId));
                                            containersSync.sync(List.of(containerId), new PageImpl<>(mappings));
                                        }),
                                        executorService)
                        ).toArray(CompletableFuture[]::new)
        );

        // Wait for both async operations to complete
        CompletableFuture.allOf(afterSaveFuture, containerSyncFuture).join();

        // Add audit logs for all updated containers
        recordAuditLogs(originalContainers, updatedContainers, DBOperationType.UPDATE);

        // Convert saved entities into response DTOs
        List<ContainerResponse> containerResponses = jsonHelper.convertValueToList(updatedContainers, ContainerResponse.class);

        // Build and return the response
        return BulkContainerResponse.builder()
                .containerResponseList(containerResponses)
                .message(prepareBulkUpdateMessage(containerResponses))
                .build();
    }

    @Override
    @Transactional
    public BulkContainerResponse deleteBulk(List<ContainerRequest> containerRequestList) {
        // Validate that all necessary container IDs are present in the request
        containerValidationUtil.validateUpdateBulkRequest(containerRequestList);

        // Extract unique container IDs from the request
        List<Long> containerIds = containerRequestList.stream()
                .map(ContainerRequest::getId)
                .distinct()
                .toList();

        // Fetch containers from DB to ensure they exist before deletion
        List<Containers> containersToDelete = containerDao.findByIdIn(containerIds);

        if (containersToDelete.isEmpty()) {
            throw new IllegalArgumentException("No containers found for the given IDs.");
        }

        // Remove associations with packings (if any)
        packingDao.removeContainersFromPacking(containerIds);

        // Delete containers from DB
        containerDao.deleteAllById(containerIds);

        // Record audit logs for the deletion operation
        recordAuditLogs(containersToDelete, null, DBOperationType.DELETE);

        // Return the response with status message
        return BulkContainerResponse.builder()
                .message(prepareBulkDeleteMessage(containersToDelete))
                .build();
    }

    private void recordAuditLogs(List<Containers> oldContainers, List<Containers> newContainers, DBOperationType operationType) {
        Map<Long, Containers> oldContainerMap = Optional.ofNullable(oldContainers).orElse(List.of()).stream()
                .filter(container -> container.getId() != null)
                .collect(Collectors.toMap(Containers::getId, Function.identity()));

        Map<Long, Containers> newContainerMap = Optional.ofNullable(newContainers).orElse(List.of()).stream()
                .filter(container -> container.getId() != null)
                .collect(Collectors.toMap(Containers::getId, Function.identity()));

        // Decide the relevant set of IDs based on operation
        Set<Long> idsToProcess = switch (operationType) {
            case CREATE -> newContainerMap.keySet();
            case DELETE -> oldContainerMap.keySet();
            case UPDATE -> {
                Set<Long> ids = new HashSet<>(oldContainerMap.keySet());
                ids.retainAll(newContainerMap.keySet()); // only intersecting IDs
                yield ids;
            }
            default -> throw new IllegalStateException("Unexpected value: " + operationType);
        };

        for (Long id : idsToProcess) {
            try {
                Containers oldData = oldContainerMap.get(id);
                Containers newData = newContainerMap.get(id);

                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId())
                                .userName(UserContext.getUser().getUsername())
                                .prevData(oldData)
                                .newData(newData)
                                .parent(Containers.class.getSimpleName())
                                .parentId(id)
                                .operation(operationType.name())
                                .build()
                );
            } catch (Exception ex) {
                log.error("Failed to add audit log for container ID {} and operation [{}]: {}", id, operationType, ex.getMessage(), ex);
            }
        }
    }

    private String prepareBulkUpdateMessage(List<ContainerResponse> containerResponses) {
        String message;

        // If more than one container was updated, return a generic bulk success message
        if (containerResponses.size() > 1) {
            message = "Bulk edit success! All selected containers have been updated.";
        } else {
            // For a single container update, include container code and optionally its containerNumber
            ContainerResponse response = containerResponses.get(0);
            String containerCode = response.getContainerCode();
            String containerNumber = response.getContainerNumber();

            message = containerNumber != null
                    ? String.format("Container %s - %s updated successfully!", containerCode, containerNumber)
                    : String.format("Container %s updated successfully!", containerCode);
        }

        return message;
    }

    private String prepareBulkDeleteMessage(List<Containers> containers) {
        String message;

        // If more than one container was deleted, return a generic bulk success message
        if (containers.size() > 1) {
            message = "Containers deleted successfully!";
        } else {
            // For a single container delete, include container code and optionally its containerNumber
            Containers response = containers.get(0);
            String containerCode = response.getContainerCode();
            String containerNumber = response.getContainerNumber();

            message = containerNumber != null
                    ? String.format("Container %s - %s deleted successfully!", containerCode, containerNumber)
                    : String.format("Container %s deleted successfully!", containerCode);
        }

        return message;
    }

    /**
     * Post-processing logic after saving a container entity.
     * <p>
     * Sets the tenant if missing, constructs a Kafka message and pushes it to the configured queue. Logs any failure during Kafka operations.
     * </p>
     *
     * @param container the container entity that was persisted
     * @param isCreate  flag indicating if this is a creation or an update
     */
    private void afterSave(Containers container, boolean isCreate) {
        try {
            // Set tenant from context if not already present on the container
            if (container.getTenantId() == null) {
                container.setTenantId(TenantContext.getCurrentTenant());
            }

            // Build Kafka message payload
            KafkaResponse kafkaResponse = producer.getKafkaResponse(container, isCreate);

            // Serialize payload to JSON
            String message = jsonHelper.convertToJson(kafkaResponse);

            // Generate a unique key for Kafka message for traceability
            String messageKey = UUID.randomUUID().toString();

            // Send message to Kafka
            producer.produceToKafka(message, senderQueue, messageKey);

            // Log success for traceability
            log.info("Pushed container update to Kafka | containerId={} | tenantId={} | key={}",
                    container.getId(), container.getTenantId(), messageKey);
        } catch (Exception ex) {
            // Log failure with detailed error message and exception stack trace
            log.error("Failed to push container update to Kafka | containerId={} | error={}",
                    container.getId(), ex.getMessage(), ex);
        }
    }

}
