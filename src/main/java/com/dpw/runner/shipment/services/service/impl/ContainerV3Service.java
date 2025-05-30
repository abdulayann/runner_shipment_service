package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.response.AttachedShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.DependentServiceHelper;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.PushToDownstreamEventDto;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.projection.ContainerDeleteInfoProjection;
import com.dpw.runner.shipment.services.projection.ContainerInfoProjection;
import com.dpw.runner.shipment.services.projection.ShipmentDetailsProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.utils.*;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.annotation.PostConstruct;
import javax.persistence.EntityNotFoundException;
import javax.servlet.http.HttpServletResponse;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;


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
    @Qualifier("executorServiceMasterData")
    ExecutorService executorServiceMasterData;

    @Autowired
    private KafkaProducer producer;

    @Autowired
    private DependentServiceHelper dependentServiceHelper;

    @Value("${containersKafka.queue}")
    private String containerKafkaQueue;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private IContainersSync containersSync;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    ExecutorService executorService;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private ContainerV3Util containerV3Util;

    @Autowired
    private IPackingV3Service packingService;

    @Autowired
    private IShipmentServiceV3 shipmentService;

    private List<String> defaultIncludeColumns = new ArrayList<>();

    @Override
    @Transactional
    public ContainerResponse create(ContainerV3Request containerRequest, String module) {
        if (containerRequest.getConsolidationId() == null && containerRequest.getShipmentsId() == null) {
            throw new ValidationException("Either ConsolidationId or ShipmentsId must be provided in the request.");
        }

        if(containerRequest.getConsolidationId() != null && containerRequest.getShipmentsId() != null){
            throw new ValidationException("Only one of ConsolidationId or ShipmentsId should be provided, not both.");
        }

        List<Containers> containersList = getSiblingContainers(containerRequest);
        containerValidationUtil.validateContainerNumberUniqueness(containerRequest.getContainerNumber(), containersList);
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting container creation | Request ID: {} | Request Body: {}", requestId, containerRequest);

        // Convert DTO to Entity
        Containers container = jsonHelper.convertValue(containerRequest, Containers.class);
        log.debug("Converted container request to entity | Entity: {}", container);

        // Save to DB
        Containers savedContainer = containerDao.save(container);
        log.info("Saved container entity to DB | Container ID: {} | Request ID: {}", savedContainer.getId(), requestId);
        handlePostSaveActions(savedContainer, containerRequest, module);

        // Audit logging
        recordAuditLogs(null, List.of(savedContainer), DBOperationType.CREATE);
        log.info("Audit log recorded for container creation | Container ID: {}", savedContainer.getId());

        ContainerResponse response = jsonHelper.convertValue(savedContainer, ContainerResponse.class);
        log.info("Returning container response | Container ID: {} | Response: {}", savedContainer.getId(), response);

        return response;
    }

    @Override
    @Transactional
    public BulkContainerResponse updateBulk(List<ContainerV3Request> containerRequestList, String module) {
        // Validate the incoming request to ensure all mandatory fields are present
        containerValidationUtil.validateUpdateBulkRequest(containerRequestList);

        // Convert the request DTOs to entity models for persistence
        List<Containers> originalContainers = jsonHelper.convertValueToList(containerRequestList, Containers.class);

        // Save the updated containers to the database
        List<Containers> updatedContainers = containerDao.saveAll(originalContainers);

        runAsyncPostSaveOperations(updatedContainers, module);

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
    @Transactional(rollbackFor = Exception.class)
    public BulkContainerResponse deleteBulk(List<ContainerV3Request> containerRequestList, String module) {
        // Validate that the request contains valid and necessary container IDs
        containerValidationUtil.validateDeleteBulkRequest(containerRequestList);

        // Extract a distinct list of container IDs from the incoming request
        List<Long> containerIds = containerRequestList.stream()
                .map(ContainerV3Request::getId).distinct().toList();

        // Retrieve the corresponding container entities from the database for validation and further processing
        List<Containers> containersToDelete = containerDao.findByIdIn(containerIds);

        // If no containers were found for the provided IDs, throw an exception to stop the process
        if (containersToDelete.isEmpty()) {
            throw new IllegalArgumentException("No containers found for the given IDs.");
        }

        // Validate that the containers are not assigned to any active shipment or packing before deletion
         validateNoAssignments(containerIds);

        // Collect all unique shipment IDs that are associated with the containers to delete
        List<Long> shipmentIds = containersToDelete.stream().map(Containers::getShipmentsList)
                .filter(Objects::nonNull)
                .flatMap(Set::stream).map(ShipmentDetails::getId).distinct().toList();

        // Proceed with the deletion of the containers and any related associations (shipment, packing, etc.)
        deleteContainerAndAssociations(containerIds, shipmentIds);

        // Log the deletion activity for auditing and tracking purposes
        recordAuditLogs(containersToDelete, null, DBOperationType.DELETE);

        // Return a response indicating the result of the bulk delete operation
        return BulkContainerResponse.builder()
                .message(prepareBulkDeleteMessage(containersToDelete))
                .build();
    }

    // Validate that containers are not assigned to both packing and cargo before deletion
    private void validateNoAssignments(List<Long> containerIds) {
        // Check if any containers are attached to both shipment cargo and packages
        List<ContainerDeleteInfoProjection> bothPackingAndCargo =
                containerDao.findContainersAttachedToBothPackingAndCargo(containerIds);

        // If containers are found with both packing and cargo assignments, throw an exception with details
        if (ObjectUtils.isNotEmpty(bothPackingAndCargo)) {
            throw new IllegalArgumentException(
                    "Selected Containers are assigned to Shipment Cargo and Packages as below, Please unassign the same to delete:\n" +
                            formatPackingAndCargoInfo(bothPackingAndCargo) // Format and display the details of affected containers
            );
        }

        // Check if any containers are attached to packing only
        List<ContainerDeleteInfoProjection> packingOnly =
                containerDao.filterContainerIdsAttachedToPacking(containerIds);

        // If containers are found with packing-only assignments, throw an exception with details
        if (ObjectUtils.isNotEmpty(packingOnly)) {
            throw new IllegalArgumentException(
                    "Selected Containers are assigned to Packages as below, Please unassign the same to delete:\n" +
                            formatPackingAndCargoInfo(packingOnly) // Format and display the details of affected containers
            );
        }

        // Check if any containers are attached to shipment cargo only
        List<ContainerDeleteInfoProjection> shipmentOnly =
                containerDao.filterContainerIdsAttachedToShipmentCargo(containerIds);

        // If containers are found with shipment cargo-only assignments, throw an exception with details
        if (ObjectUtils.isNotEmpty(shipmentOnly)) {
            throw new IllegalArgumentException(
                    "Selected Containers are assigned to Shipment Cargo as below, Please unassign the same to delete:\n" +
                            formatCargoOnlyInfo(shipmentOnly) // Format and display the details of affected containers
            );
        }
    }

    // Helper method to format the information about containers attached to both packing and cargo
    private String formatPackingAndCargoInfo(List<ContainerDeleteInfoProjection> projections) {
        return projections.stream()
                .map(p -> String.format("Container Number: %s - Shipment Number: %s - Packages: %s",
                        p.getContainerNumber(), p.getShipmentId(), p.getPacks())) // Format each container's details
                .collect(Collectors.joining("\n")); // Join all container details with a newline for display
    }

    // Helper method to format the information about containers attached only to shipment cargo
    private String formatCargoOnlyInfo(List<ContainerDeleteInfoProjection> projections) {
        return projections.stream()
                .map(p -> String.format("Container Number: %s - Shipment Number: %s",
                        p.getContainerNumber(), p.getShipmentId())) // Format each container's details without packages
                .collect(Collectors.joining("\n")); // Join all container details with a newline for display
    }

    // Method to handle the deletion of containers and their associated entities
    private void deleteContainerAndAssociations(List<Long> containerIds, List<Long> shipmentIds) {
        // Remove containers from packing associations
        packingService.removeContainersFromPacking(containerIds);

        // Detach the containers from any associated shipments
        shipmentsContainersMappingDao.detachListShipments(containerIds, shipmentIds, false);

        // Delete the containers from the database
        containerDao.deleteAllById(containerIds);
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
        processAuditLogIds(idsToProcess, oldContainerMap, newContainerMap, operationType);
    }

    public void processAuditLogIds(Set<Long> idsToProcess, Map<Long, Containers> oldContainerMap, Map<Long, Containers> newContainerMap, DBOperationType operationType) {
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

    protected List<Containers> getSiblingContainers(ContainerV3Request containerRequest) {
        if (containerRequest.getConsolidationId() != null) {
            return containerDao.findByConsolidationId(containerRequest.getConsolidationId());
        } else if (containerRequest.getShipmentsId() != null) {
            Long shipmentId = containerRequest.getShipmentsId();
            return containerDao.findByShipmentId(shipmentId);
        } else if (containerRequest.getBookingId() != null) {
            return containerDao.findByBookingIdIn(List.of(containerRequest.getBookingId()));
        }
        return new ArrayList<>();
    }

    /**
     * Post-processing logic invoked after saving a container entity.
     * <p>
     * Ensures the tenant ID is set on the container if missing,
     * then constructs and pushes a Kafka message to the configured internal queue.
     * Any failures during Kafka operations are logged.
     * </p>
     *
     * @param container the container entity that was persisted
     * @param isCreate  true if the container was newly created; false if updated
     */
    private void afterSave(Containers container, boolean isCreate) {
        try {
            // Set tenant from context if not already present on the container
            if (container.getTenantId() == null) {
                container.setTenantId(TenantContext.getCurrentTenant());
            }

            triggerPushToDownStream(container, isCreate, Constants.CONTAINER_AFTER_SAVE);

        } catch (Exception ex) {
            // Log failure with detailed error message and exception stack trace
            log.error("Failed to push container update to Kafka | containerId={} | error={}",
                    container.getId(), ex.getMessage(), ex);
        }
    }

    private void triggerPushToDownStream(Containers container, Boolean isCreate, String sourceInfo) {
        String transactionId = UUID.randomUUID().toString();

        log.info("[InternalKafkaPush] Initiating downstream internal Kafka push | containerId={} | isCreate={} | transactionId={}",
                container.getId(), isCreate, transactionId);

        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(container.getId())
                .parentEntityName(CONTAINER)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .sourceInfo(sourceInfo)
                        .tenantId(container.getTenantId())
                        .isCreate(isCreate).build()).build();

        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, transactionId);

        log.info("[InternalKafkaPush] Message successfully pushed to internal Kafka | containerId={} | transactionId={}",
                container.getId(), transactionId);
    }

    @Override
    public ContainerSummaryResponse calculateContainerSummary(Long shipmentId, Long consolidationId, String xSource) throws RunnerException {
        if (shipmentId == null && consolidationId == null) {
            throw new RunnerException("Please provide shipmentId and consolidationId for containers summary");
        }
        if (shipmentId != null) {
            List<Containers> containers;
            if (Objects.equals(xSource, NETWORK_TRANSFER))
                containers = containerDao.findByShipmentIdWithoutTenantFilter(shipmentId);
            else
                containers = containerDao.findByShipmentId(shipmentId);
            List<Containers> containersList = new ArrayList<>(containers);
            return getContainerSummaryResponse(containersList, true, xSource);
        }
        List<Containers> containers;
        if (Objects.equals(xSource, NETWORK_TRANSFER))
            containers = containerDao.findByConsolidationIdWithoutTenantFilter(consolidationId);
        else
            containers = containerDao.findByConsolidationId(consolidationId);
        List<Containers> containersList = new ArrayList<>(containers);
        return getContainerSummaryResponse(containersList, false, xSource);
    }

    @Override
    public List<Containers> findByIdIn(List<Long> containerIds) {
        return containerRepository.findByIdIn(containerIds);
    }

    @Override
    public ContainerListResponse fetchShipmentContainers(ListCommonRequest request, String xSource) throws RunnerException {
        if (StringUtility.isEmpty(request.getEntityId())) {
            throw new ValidationException("Entity id is empty");
        }
        ListCommonRequest listCommonRequest;
        if (CollectionUtils.isEmpty(request.getFilterCriteria())) {
            listCommonRequest = CommonUtils.constructListCommonRequest(SHIPMENTS_LIST, Long.valueOf(request.getEntityId()), Constants.CONTAINS);
        } else {
            listCommonRequest = CommonUtils.andCriteria(SHIPMENTS_LIST, Long.valueOf(request.getEntityId()), Constants.CONTAINS, request);
        }
        listCommonRequest.setSortRequest(request.getSortRequest());
        listCommonRequest.setPageNo(request.getPageNo());
        listCommonRequest.setPageSize(request.getPageSize());
        listCommonRequest.setContainsText(request.getContainsText());
        ContainerListResponse containerListResponse = list(listCommonRequest, true, xSource);
        log.info("Container detail list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
        containerListResponse.setTotalPages(containerListResponse.getTotalPages());
        containerListResponse.setNumberOfRecords(containerListResponse.getNumberOfRecords());

        return containerListResponse;

    }

    @Override
    public ContainerListResponse fetchConsolidationContainers(ListCommonRequest request, String xSource) throws RunnerException {
        ListCommonRequest enrichedRequest;

        if (CollectionUtils.isEmpty(request.getFilterCriteria())) {
            enrichedRequest = CommonUtils.constructListCommonRequest(
                    CONSOLIDATION_ID, Long.valueOf(request.getEntityId()), Constants.EQ);
        } else {
            enrichedRequest = CommonUtils.andCriteria(
                    CONSOLIDATION_ID, Long.valueOf(request.getEntityId()), Constants.EQ, request);
        }

        // Carry forward pagination, sorting, and search text
        enrichedRequest.setSortRequest(request.getSortRequest());
        enrichedRequest.setPageNo(request.getPageNo());
        enrichedRequest.setPageSize(request.getPageSize());
        enrichedRequest.setContainsText(request.getContainsText());

        ContainerListResponse containerListResponse;
        try {
            containerListResponse = list(enrichedRequest, true, xSource);
        } catch (Exception ex) {
            throw new IllegalArgumentException("Failed to fetch consolidation containers", ex);
        }

        List<ContainerBaseResponse> containers = containerListResponse.getContainers();
        if (CollectionUtils.isEmpty(containers)) {
            log.info("No containers found for consolidation.");
            return containerListResponse;
        }

        List<Long> containerIds = containers.stream().map(ContainerBaseResponse::getId)
                .filter(Objects::nonNull).distinct().toList();

        List<ShipmentDetailsProjection> attachedShipmentDetails = shipmentService.findShipmentDetailsByAttachedContainerIds(containerIds);

        Map<Long, List<ShipmentDetailsProjection>> containerIdToShipmentDetailsMap =
                attachedShipmentDetails.stream()
                        .filter(Objects::nonNull)
                        .collect(Collectors.groupingBy(ShipmentDetailsProjection::getContainerId));

        containers.forEach(container -> {
            List<ShipmentDetailsProjection> details = containerIdToShipmentDetailsMap.get(container.getId());

            if (ObjectUtils.isNotEmpty(details)) {
                List<AttachedShipmentResponse> attachedShipmentResponseList = details.stream()
                        .map(detail -> AttachedShipmentResponse.builder()
                                .attachedShipmentId(detail.getId())
                                .attachedShipmentNumber(detail.getShipmentNumber())
                                .attachedShipmentType(detail.getShipmentType())
                                .build())
                        .toList();

                container.setAttachedShipmentResponses(attachedShipmentResponseList);
            }
        });

        return containerListResponse;
    }

    private void setAssignedContainer(ContainerListResponse containerListResponse, String xSource) {
        List<Long> containersId = containerListResponse.getContainers().stream()
                .map(ContainerBaseResponse::getId) // or .map(container -> container.getId())
                .filter(Objects::nonNull)
                .toList();
        if (!CollectionUtils.isEmpty(containersId)) {
            List<Packing> packs;
            if (Objects.equals(xSource, NETWORK_TRANSFER))
                packs = packingDao.findByContainerIdInWithoutTenantFilter(containersId);
            else
                packs = packingDao.findByContainerIdIn(containersId);
            AtomicLong assignedContainerCount = new AtomicLong(0L);
            AtomicLong unassignedContainerCount = new AtomicLong(0L);
            Set<Long> uniqueContainers = packs.stream().map(Packing::getContainerId).collect(Collectors.toSet());
            containerListResponse.getContainers().forEach(containerBaseResponse -> {
                if (uniqueContainers.contains(containerBaseResponse.getId())) {
                    containerBaseResponse.setAssignedContainer(Constants.YES);
                    assignedContainerCount.getAndIncrement();
                } else {
                    containerBaseResponse.setAssignedContainer(Constants.NO);
                    unassignedContainerCount.getAndIncrement();
                }
            });
            containerListResponse.setAssignedContainerCount(assignedContainerCount.get());
            containerListResponse.setUnassignedContainerCount(unassignedContainerCount.get());
        }
    }

    private List<ContainerBaseResponse> convertEntityListWithFieldFilter(List<Containers> lst, List<String> includeColumns) {
        List<ContainerBaseResponse> responseList = new ArrayList<>();
        long start = System.currentTimeMillis();
        lst.forEach(containers -> responseList.add(
                (ContainerBaseResponse) commonUtils.setIncludedFieldsToResponse(containers, new HashSet<>(includeColumns), new ContainerBaseResponse())));
        log.info("Total time take to set container response {} ms", (System.currentTimeMillis() - start));
        return responseList;
    }

    public ContainerSummaryResponse getContainerSummaryResponse(List<Containers> containersList, boolean isShipment, String xSource) throws RunnerException {
        double totalWeight = 0;
        double packageCount = 0;
        double tareWeight = 0;
        double totalVolume = 0;
        double totalContainerCount = 0;
        double totalPacks = 0;
        int dgContainers = 0;
        double netWeight = 0;
        long assignedContainers = 0;
        List<ContainerSummaryResponse.GroupedContainerSummary> groupedContainerSummaryList = new ArrayList<>();
        String toWeightUnit = Constants.WEIGHT_UNIT_KG;
        String toVolumeUnit = Constants.VOLUME_UNIT_M3;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        if (!isStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        if (!isStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        String finalToWeightUnit = toWeightUnit;
        if (containersList != null) {
            Map<String, ContainerSummaryResponse.GroupedContainerSummary> summaryMap = new TreeMap<>();
            for (Containers containers : containersList) {
                dgContainers = getDgContainers(containers, dgContainers);
                double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                double netWtDef = convertUnit(Constants.MASS, containers.getNetWeight(), containers.getNetWeightUnit(), toWeightUnit).doubleValue();
                double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                totalWeight = totalWeight + wInDef;
                tareWeight = tareWeight + tarDef;
                netWeight = netWeight + netWtDef;
                packageCount = getTotalPacks(containers, packageCount);
                totalVolume = totalVolume + volume;
                totalContainerCount = getTotalContainerCount(containers, totalContainerCount);
                totalPacks = getTotalPacks(containers, totalPacks);

                ContainerSummaryResponse.GroupedContainerSummary summary = summaryMap.computeIfAbsent(containers.getContainerCode(), k -> {
                    ContainerSummaryResponse.GroupedContainerSummary s = new ContainerSummaryResponse.GroupedContainerSummary();
                    s.setContainerCode(k);
                    s.setContainerCount("0");
                    s.setTotalWeight(0);
                    s.setTotalNetWeight(0);
                    s.setTotalWeightUnit(finalToWeightUnit);
                    s.setTotalNetWeightUnit(finalToWeightUnit);
                    return s;
                });

                summary.setContainerCount(String.valueOf(Long.parseLong(summary.getContainerCount()) +
                        (containers.getContainerCount() != null ? containers.getContainerCount() : 0)));
                summary.setTotalWeight(summary.getTotalWeight() + wInDef);
                summary.setTotalNetWeight(summary.getTotalNetWeight() + netWtDef);
            }
            if (!isShipment) {
                List<ShipmentsContainersMapping> shipmentsContainersMappings;
                if (Objects.equals(xSource, NETWORK_TRANSFER))
                    shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerIdInWithoutTenantFilter(containersList.stream().map(BaseEntity::getId).toList());
                else
                    shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerIdIn(containersList.stream().map(BaseEntity::getId).toList());
                assignedContainers = shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getContainerId).distinct().count();
            }
            groupedContainerSummaryList = new ArrayList<>(summaryMap.values());
        }
        ContainerSummaryResponse response = new ContainerSummaryResponse();
        setTotalPackages(isShipment, totalPacks, v1TenantSettingsResponse, response);
        response.setTotalContainers(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalContainerCount), 0, v1TenantSettingsResponse));
        response.setTotalWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalTareWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(tareWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalNetWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(netWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalContainerVolume(String.format(Constants.STRING_FORMAT, IReport.convertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), v1TenantSettingsResponse), toVolumeUnit));
        if (response.getSummary() == null)
            response.setSummary("");
        setContainerSummary(containersList, response);
        response.setDgContainers(dgContainers);
        setAssignedContainerCount(containersList, isShipment, assignedContainers, v1TenantSettingsResponse, response);
        response.setGroupedContainersSummary(groupedContainerSummaryList);
        return response;
    }

    private static void setTotalPackages(boolean isShipment, double totalPacks, V1TenantSettingsResponse v1TenantSettingsResponse, ContainerSummaryResponse response) {
        if (isShipment) {
            response.setTotalPackages(String.format(Constants.STRING_FORMAT, IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalPacks), 0, v1TenantSettingsResponse), Constants.PACKAGES));
        } else {
            response.setTotalPackages(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalPacks), 0, v1TenantSettingsResponse));
        }
    }

    private void setAssignedContainerCount(List<Containers> containersList, boolean isShipment, long assignedContainers, V1TenantSettingsResponse v1TenantSettingsResponse, ContainerSummaryResponse response) {
        if (isShipment && !CollectionUtils.isEmpty(containersList)) {
            List<Long> containerIds = containersList.stream().map(Containers::getId).toList();
            List<Packing> packings = packingDao.findByContainerIdIn(containerIds);
            Set<Long> containersId = packings.stream()
                    .map(Packing::getContainerId) // or .map(container -> container.getId())
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
            response.setAssignedContainersCount(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(containersId.size()), 0, v1TenantSettingsResponse));
        } else {
            response.setAssignedContainersCount(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(assignedContainers), 0, v1TenantSettingsResponse));
        }
    }

    private void setContainerSummary(List<Containers> containersList, ContainerSummaryResponse response) {
        try {
            response.setSummary(getContainerSummary(containersList));
        } catch (Exception e) {
            log.error("Error calculating summary");
        }
    }

    private double getTotalPacks(Containers containers, double totalPacks) {
        if (!isStringNullOrEmpty(containers.getPacks()))
            totalPacks = totalPacks + Long.parseLong(containers.getPacks());
        return totalPacks;
    }

    private double getTotalContainerCount(Containers containers, double totalContainerCount) {
        if (containers.getContainerCount() != null)
            totalContainerCount = totalContainerCount + containers.getContainerCount();
        return totalContainerCount;
    }

    private int getDgContainers(Containers containers, int dgContainers) {
        if (Boolean.TRUE.equals(containers.getHazardous()))
            dgContainers += containers.getContainerCount() != null ? containers.getContainerCount().intValue() : 0;
        return dgContainers;
    }

    public String getContainerSummary(List<Containers> response) {
        if (response == null || response.isEmpty()) return null;

        // Sort
        response.sort(Comparator.comparing(Containers::getContainerCode));

        // Group by containerCode and sum counts
        Map<String, Long> containerCodeCountMap = response.stream()
                .collect(Collectors.groupingBy(
                        Containers::getContainerCode,
                        Collectors.summingLong(Containers::getContainerCount)
                ));

        // Total count
        long totalCount = containerCodeCountMap.values().stream().mapToLong(Long::longValue).sum();

        // Build summary
        return totalCount + " (" + containerCodeCountMap.entrySet().stream()
                .map(e -> e.getKey() + "*" + e.getValue())
                .collect(Collectors.joining(", ")) + ")";
    }

    @Override
    public ContainerNumberCheckResponse validateContainerNumber(String containerNumber) {
        ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
        response.setLastDigit(null);
        response.setIsLastDigitCorrect(null);
        if (containerNumber.length() != 10 && containerNumber.length() != 11) {
            checkUnavailableContainerMD(containerNumber, response); // unavailable cont master data would be around 4-5 characters max (acc. to story), so only checking if it's not 10 or 11
            return response;
        }
        ContainerNumberCheckResponse validationResponse = validateContainerNumberFormat(containerNumber, response);
        if (validationResponse != null) return validationResponse;
        List<Integer> eqvNumValue = assignEquivalentNumberValue();
        int expandedVal = getExpandedVal(containerNumber, eqvNumValue);
        int checkDigit = expandedVal % 11;
        if (checkDigit == 10)
            checkDigit = 0;
        if (containerNumber.length() == 11) {
            if (checkDigit != containerNumber.charAt(10) - 48) {
                response.setSuccess(false);
                response.setIsLastDigitCorrect(false);
                return response;
            }
        } else response.setLastDigit(checkDigit);
        response.setSuccess(true);
        return response;
    }

    public void checkUnavailableContainerMD(String containerNumber, ContainerNumberCheckResponse response) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> subCriteria1 = Arrays.asList(
                List.of(MasterDataConstants.ITEM_TYPE),
                "=",
                "131"
        );
        List<Object> subCriteria2 = Arrays.asList(
                List.of(MasterDataConstants.ITEM_VALUE),
                "=",
                containerNumber
        );
        List<Object> criteria = new ArrayList<>(List.of(subCriteria1, "and", subCriteria2));
        request.setCriteriaRequests(criteria);
        V1DataResponse v1DataResponse = v1Service.fetchMasterData(request);
        List<MasterData> entityTransferMasterLists = jsonHelper.convertValueToList(v1DataResponse.entities, MasterData.class);
        response.setSuccess(!listIsNullOrEmpty(entityTransferMasterLists));
    }

    protected ContainerNumberCheckResponse validateContainerNumberFormat(String containerNumber, ContainerNumberCheckResponse response) {
        for (int i = 0; i < 4; i++) {
            if (containerNumber.charAt(i) < 65 || containerNumber.charAt(i) > 90) {
                response.setSuccess(false);
                return response;
            }
        }
        for (int i = 4; i < 10; i++) {
            if (containerNumber.charAt(i) < 48 || containerNumber.charAt(i) > 57) {
                response.setSuccess(false);
                return response;
            }
        }
        if (containerNumber.length() == 11 && (containerNumber.charAt(10) < 48 || containerNumber.charAt(10) > 57)) {
            response.setSuccess(false);
            return response;
        }
        return null;
    }

    private List<Integer> assignEquivalentNumberValue() {
        List<Integer> eqvNumValue = new ArrayList<>();
        int val = 10;

        for (int i = 0; i < 26; i++) {
            if (val % 11 == 0)
                val++;

            eqvNumValue.add(val);
            val++;
        }

        return eqvNumValue;
    }

    private int getExpandedVal(String containerNumber, List<Integer> eqvNumValue) {
        int expandedVal = 0;
        for (int i = 0; i < 4; i++) {
            expandedVal = expandedVal + (int) Math.pow(2, i) * eqvNumValue.get(containerNumber.charAt(i) - 65);
        }
        for (int i = 4; i < 10; i++) {
            expandedVal = expandedVal + (int) Math.pow(2, i) * (containerNumber.charAt(i) - 48);
        }
        return expandedVal;
    }

    @Override
    public void downloadContainers(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws RunnerException {
        containerV3Util.downloadContainers(response, request);
    }

    @Override
    public ContainerListResponse list(ListCommonRequest request, boolean getMasterData, String xSource) throws RunnerException {
        try {
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage;
            if (Objects.equals(xSource, NETWORK_TRANSFER))
                containersPage = containerDao.findAllWithoutTenantFilter(tuple.getLeft(), tuple.getRight());
            else
                containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Containers list for get containers retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            List<String> includeColumns;
            if (CollectionUtils.isEmpty(request.getIncludeColumns())) {
                includeColumns = defaultIncludeColumns;
            } else {
                includeColumns = request.getIncludeColumns();
            }

            List<ContainerBaseResponse> responseList = convertEntityListWithFieldFilter(containersPage.getContent(), includeColumns);
            Map<String, Object> masterDataResponse = this.getMasterDataForList(responseList, getMasterData);
            ContainerListResponse containerListResponse = ContainerListResponse.builder()
                    .containers(responseList)
                    .numberOfRecords(containersPage.getTotalElements())
                    .totalPages(containersPage.getTotalPages())
                    .masterData(masterDataResponse)
                    .build();

            setAssignedContainer(containerListResponse, xSource);
            return containerListResponse;
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    private Map<String, Object> getMasterDataForList(List<ContainerBaseResponse> responseList, boolean getMasterData) {
        Map<String, Object> masterDataResponse = new HashMap<>();
        if (getMasterData) {
            try {
                double startTime = System.currentTimeMillis();
                var locationDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> containerV3Util.addAllUnlocationInSingleCallList(responseList, masterDataResponse)), executorServiceMasterData);
                var masterDataFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> containerV3Util.addAllMasterDataInSingleCallList(responseList, masterDataResponse)), executorServiceMasterData);
                var commodityTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> containerV3Util.addAllCommodityTypesInSingleCall(responseList, masterDataResponse)), executorServiceMasterData);
                var containerTypeFuture = CompletableFuture.runAsync(masterDataUtils.withMdc(() -> containerV3Util.addAllContainerTypesInSingleCall(responseList, masterDataResponse)), executorServiceMasterData);
                CompletableFuture.allOf(locationDataFuture, masterDataFuture, commodityTypeFuture, containerTypeFuture).join();
                log.info("Time taken to fetch Master-data for event:{} | Time: {} ms. || RequestId: {}", LoggerEvent.CONTAINER_LIST_MASTER_DATA, (System.currentTimeMillis() - startTime), LoggerHelper.getRequestIdFromMDC());
            } catch (Exception ex) {
                log.error(Constants.ERROR_OCCURRED_FOR_EVENT, LoggerHelper.getRequestIdFromMDC(), IntegrationType.MASTER_DATA_FETCH_FOR_CONTAINER_LIST, ex.getLocalizedMessage());
            }
        }
        return masterDataResponse;
    }

    private void handlePostSaveActions(Containers container, ContainerV3Request request, String module) {
        if (!Set.of(SHIPMENT, CONSOLIDATION).contains(module)) return;

        List<CompletableFuture<Void>> futures = new ArrayList<>();

        futures.add(CompletableFuture.runAsync(
                masterDataUtils.withMdc(() -> afterSave(container, true)),
                executorService
        ));

        Optional.ofNullable(module)
            .filter(SHIPMENT::equals)
            .filter(m -> container.getId() != null)
            .filter(m -> request.getShipmentsId() != null)
            .ifPresent(m -> shipmentsContainersMappingDao.assignShipments(
                container.getId(),
                Set.of(request.getShipmentsId()),
                false
            ));


        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
    }

    private void runAsyncPostSaveOperations(List<Containers> containers, String module) {
        if (!Set.of(SHIPMENT, CONSOLIDATION).contains(module)) return;
        CompletableFuture<Void> afterSaveFuture = runAfterSaveAsync(containers);
        CompletableFuture.allOf(afterSaveFuture).join();
    }

    private CompletableFuture<Void> runAfterSaveAsync(List<Containers> containers) {
        return CompletableFuture.allOf(
                containers.stream()
                        .map(container -> CompletableFuture.runAsync(
                                masterDataUtils.withMdc(() -> afterSave(container, false)),
                                executorService
                        ))
                        .toArray(CompletableFuture[]::new)
        );
    }

    @Override
    public void processContainersAfterShipmentAttachment(
            Long consolidationId,
            List<ShipmentDetails> shipmentDetailsList,
            Set<Long> attachedShipmentIds,
            Set<Long> interBranchRequestedShipIds) {

        log.info("Processing container updates for consolidationId: {}", consolidationId);

        shipmentDetailsList.stream()
                // Filter eligible shipments:
                // - Must be attached
                // - Must not be part of inter-branch shipment request
                // - Must have containers to process
                .filter(shipment -> attachedShipmentIds.contains(shipment.getId()) &&
                        !interBranchRequestedShipIds.contains(shipment.getId()) &&
                        shipment.getContainersList() != null)
                .map(shipment -> new ArrayList<>(shipment.getContainersList()))
                .forEach(containers -> {
                    // Set consolidationId for each container
                    containers.forEach(container -> container.setConsolidationId(consolidationId));

                    // Save updated containers
                    List<Containers> saved = containerDao.saveAll(containers);

                    // Perform post-save actions
                    afterSaveList(saved, false);

                    log.info("Updated and saved {} containers for consolidationId: {}", saved.size(), consolidationId);
                });
    }

    public void afterSaveList(List<Containers> containers, boolean isCreate) {
        if (containers != null && !containers.isEmpty()) {
            for (Containers container : containers) {
                afterSave(container, isCreate);
            }
        }
    }


    @PostConstruct
    private void setDefaultIncludeColumns() {
        defaultIncludeColumns = FieldUtils.getNonRelationshipFields(Containers.class);
        defaultIncludeColumns.addAll(List.of("id", "guid", "tenantId"));
    }

    @Transactional
    @Override
    public ContainerResponse assignContainers(AssignContainerRequest request) throws RunnerException {
        // make sure pack ids is empty (never null)
        request.setShipmentPackIds(request.getShipmentPackIds().entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        e -> e.getValue() == null ? new ArrayList<>() : e.getValue()
                )));

        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        Map<Long, Packing> packingListMap = new HashMap<>();
        List<Packing> assignedPacks = new ArrayList<>();
        List<ShipmentsContainersMapping> shipmentsContainersMappings = new ArrayList<>();
        Set<Long> assignedShipIds = new HashSet<>();

        // fetch data
        Containers container = fetchDataForAssignContainer(request, shipmentDetailsMap,
                                        packingListMap, assignedPacks, shipmentsContainersMappings, assignedShipIds);

        List<Long> shipmentIdsToSetContainerCargo = new ArrayList<>();

        // validate before assign container
        containerValidationUtil.validateBeforeAssignContainer(shipmentDetailsMap);

        // Do calculations/logic implementation
        List<Long> shipmentIdsForAttachment = assignContainerCalculationsAndLogic(shipmentDetailsMap, assignedShipIds, request,
                                                                shipmentIdsToSetContainerCargo, container, packingListMap, assignedPacks);

        // Save the data
        container = saveAssignContainerResults(shipmentIdsToSetContainerCargo, packingListMap, container, shipmentIdsForAttachment);

        return jsonHelper.convertValue(container, ContainerResponse.class);
    }

    private Containers fetchDataForAssignContainer(AssignContainerRequest request,
                                                   Map<Long, ShipmentDetails> shipmentDetailsMap, Map<Long, Packing> packingListMap,
                                                   List<Packing> assignedPacks, List<ShipmentsContainersMapping> shipmentsContainersMappings,
                                                   Set<Long> assignedShipIds) {
        // Identify requests
        Long containerId = request.getContainerId();
        Set<Long> shipmentIdsRequestedList = request.getShipmentPackIds().keySet();
        List<Long> packIdsRequestedList = request.getShipmentPackIds().values()
                .stream()
                .filter(Objects::nonNull)
                .flatMap(List::stream)
                .filter(Objects::nonNull).toList();

        Set<Long> shipmentIds = new HashSet<>(shipmentIdsRequestedList);

        // Fetch data already assigned
        assignedPacks.addAll(packingDao.findByContainerIdIn(List.of(containerId)).stream().toList());
        shipmentsContainersMappings.addAll(shipmentsContainersMappingDao.findByContainerId(containerId));
        assignedShipIds.addAll(shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList());

        shipmentIds.addAll(assignedShipIds);

        // Fetch data to be assigned
        Containers container = containerDao.findById(containerId)
                .orElseThrow(() -> new EntityNotFoundException("Container not found with ID: " + containerId));
        if (!listIsNullOrEmpty(packIdsRequestedList)) {
            packingListMap.putAll(packingDao.findByIdIn(packIdsRequestedList).stream().collect(Collectors.toMap(Packing::getId, Function.identity())));
        }

        // Fetch all shipments (already assigned and to be assigned)
        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentIds);
        shipmentDetailsMap.putAll(shipmentDetails.stream().collect(Collectors.toMap(ShipmentDetails::getId, Function.identity())));

        // assigning zero to weight and volume as it will be freshly recalculated
        containerV3Util.resetContainerDataForRecalculation(container);

        return container;
    }

    protected List<Long> assignContainerCalculationsAndLogic(Map<Long, ShipmentDetails> shipmentDetailsMap, Set<Long> assignedShipIds,
                                                           AssignContainerRequest request, List<Long> shipmentIdsToSetContainerCargo,
                                                           Containers container, Map<Long, Packing> packingListMap,
                                                           List<Packing> assignedPacks) throws RunnerException {
        List<Long> shipmentIdsForAttachment = new ArrayList<>();
        for (Long id : request.getShipmentPackIds().keySet()) {
            ShipmentDetails shipmentDetails = shipmentDetailsMap.get(id);
            if (!assignedShipIds.contains(id)) {
                shipmentIdsForAttachment.add(id); // need to assign this shipment
            }
            if (listIsNullOrEmpty(request.getShipmentPackIds().get(id))) { // zero packages came for this shipment
                assignContainerOnlyToShipment(shipmentDetails, container, shipmentIdsToSetContainerCargo);
            } else { // assigning some packages
                assignContainerToShipmentAndPackages(shipmentDetails, request, container, packingListMap);
            }
        }
        if (!listIsNullOrEmpty(assignedPacks)) { // adding weight/volume of already assigned packs
            for (Packing assignedPack : assignedPacks) {
                addPackageDataToContainer(container, assignedPack);
            }
        }
        for(Long id: assignedShipIds) { // adding weight/volume of already assigned Shipment Cargo
            ShipmentDetails shipmentDetails = shipmentDetailsMap.get(id);
            addShipmentCargoToContainer(container, shipmentDetails);
        }
        containerV3Util.setContainerNetWeight(container); // set container gross weight from cargo weight (net weight) and tare weight
        return shipmentIdsForAttachment;
    }

    private void assignContainerOnlyToShipment(ShipmentDetails shipmentDetails, Containers container,
                                               List<Long> shipmentIdsToSetContainerCargo) throws RunnerException {
        if (shipmentDetails.getContainerAssignedToShipmentCargo() == null &&
                !checkIfAnyPackIsAssignedToContainer(shipmentDetails)) { // if both cargo summary and package not attached to any container
            shipmentIdsToSetContainerCargo.add(shipmentDetails.getId()); // assign container to shipment cargo
            addShipmentCargoToContainer(container, shipmentDetails);
        }
    }

    @Override
    public void addShipmentCargoToContainerInCreateFromBooking(Containers container, CustomerBookingV3Request customerBookingV3Request) throws RunnerException {
        container.setGrossWeight(containerV3Util.getAddedWeight(container.getGrossWeight(), container.getGrossWeightUnit(), customerBookingV3Request.getGrossWeight(), customerBookingV3Request.getGrossWeightUnit()));
        container.setGrossVolume(containerV3Util.getAddedVolume(container.getGrossVolume(), container.getGrossVolumeUnit(), customerBookingV3Request.getVolume(), customerBookingV3Request.getVolumeUnit()));
        containerV3Util.addNoOfPackagesToContainer(container, customerBookingV3Request.getQuantity(), customerBookingV3Request.getQuantityUnit());
    }

    @Override
    public void addShipmentCargoToContainer(Containers container, ShipmentDetails shipmentDetails) throws RunnerException {
        container.setGrossWeight(containerV3Util.getAddedWeight(container.getGrossWeight(), container.getGrossWeightUnit(), shipmentDetails.getWeight(), shipmentDetails.getWeightUnit()));
        container.setGrossVolume(containerV3Util.getAddedVolume(container.getGrossVolume(), container.getGrossVolumeUnit(), shipmentDetails.getVolume(), shipmentDetails.getVolumeUnit()));
        containerV3Util.addNoOfPackagesToContainer(container, shipmentDetails.getNoOfPacks(), shipmentDetails.getPacksUnit());
    }

    private void assignContainerToShipmentAndPackages(ShipmentDetails shipmentDetails, AssignContainerRequest request, Containers container,
                                                      Map<Long, Packing> packingListMap) throws RunnerException {
        containerValidationUtil.validateCanAssignPackageToContainer(shipmentDetails);
        for (Long packingId : request.getShipmentPackIds().get(shipmentDetails.getId())) { // assigning new packs and adding its weight/volume
            Packing packing = packingListMap.get(packingId);
            packing.setContainerId(container.getId());
            addPackageDataToContainer(container, packing);
        }
    }

    private void addPackageDataToContainer(Containers container, Packing packing) throws RunnerException {
        container.setGrossWeight(containerV3Util.getAddedWeight(container.getGrossWeight(), container.getGrossWeightUnit(), packing.getWeight(), packing.getWeightUnit()));
        container.setGrossVolume(containerV3Util.getAddedVolume(container.getGrossVolume(), container.getGrossVolumeUnit(), packing.getVolume(), packing.getVolumeUnit()));
        containerV3Util.addNoOfPackagesToContainerFromPacks(container, packing.getPacks(), packing.getPacksType());
    }

    private Containers saveAssignContainerResults(List<Long> shipmentIdsToSetContainerCargo, Map<Long, Packing> packingListMap,
                                                  Containers container, List<Long> shipmentIdsForAttachment) {
        if (!listIsNullOrEmpty(shipmentIdsToSetContainerCargo))
            shipmentDao.setShipmentIdsToContainer(shipmentIdsToSetContainerCargo, container.getId());
        if (!packingListMap.isEmpty() && !listIsNullOrEmpty(packingListMap.values().stream().toList()))
            packingDao.saveAll(packingListMap.values().stream().toList());
        container = containerDao.save(container);

        // assign shipment to containers
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>();
        for (Long id : shipmentIdsForAttachment) {
            ShipmentsContainersMapping entity = new ShipmentsContainersMapping();
            entity.setShipmentId(id);
            entity.setContainerId(container.getId());
            shipmentsContainersMappingList.add(entity);
        }
        if (!listIsNullOrEmpty(shipmentsContainersMappingList))
            shipmentsContainersMappingDao.saveAll(shipmentsContainersMappingList);
        return container;
    }

    private boolean checkIfAnyPackIsAssignedToContainer(ShipmentDetails shipmentDetails) {
        for (Packing packing : shipmentDetails.getPackingList()) {
            if (packing.getContainerId() != null)
                return true;
        }
        return false;
    }

    @Override
    public List<Long> findContainerIdsAttachedToEitherPackingOrShipment(List<Long> containerIds) {
        return containerDao.findContainerIdsAttachedToEitherPackingOrShipment(containerIds);
    }

    @Override
    public List<ContainerInfoProjection> getContainers(List<Long> containerIds) {
        return containerDao.findByContainerIds(containerIds);
    }

    @Override
    @Transactional
    public ContainerResponse unAssignContainers(UnAssignContainerRequest request) throws RunnerException {
        // make sure pack ids is empty (never null)
        request.setShipmentPackIds(request.getShipmentPackIds().entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        e -> e.getValue() == null ? new ArrayList<>() : e.getValue()
                )));

        Map<Long, ShipmentDetails> shipmentDetailsMap = new HashMap<>();
        Map<Long, List<Packing>> shipmentPackingMap = new HashMap<>();
        List<ShipmentsContainersMapping> shipmentsContainersMappings = new ArrayList<>();

        // fetch data
        Containers container = fetchDataForUnAssignContainer(request.getContainerId(), shipmentDetailsMap, shipmentPackingMap, shipmentsContainersMappings);

        List<Long> shipmentIdsForCargoDetachment = new ArrayList<>();
        List<Long> removeAllPackingIds = new ArrayList<>();

        // Do calculations/logic implementation
        List<Long> shipmentIdsForDetachment = unAssignContainerCalculationsAndLogic(request, container, shipmentDetailsMap, shipmentPackingMap,
                                                                                    shipmentIdsForCargoDetachment, removeAllPackingIds);

        // Save the data
        container = saveUnAssignContainerResults(shipmentIdsForDetachment, removeAllPackingIds, shipmentIdsForCargoDetachment,
                                                    container, shipmentsContainersMappings);

        return jsonHelper.convertValue(container, ContainerResponse.class);
    }

    private Containers fetchDataForUnAssignContainer(Long containerId, Map<Long, ShipmentDetails> shipmentDetailsMap,
                                                     Map<Long, List<Packing>> shipmentPackingMap, List<ShipmentsContainersMapping> shipmentsContainersMappings) {
        // Fetch container data
        Containers container = containerDao.findById(containerId)
                .orElseThrow(() -> new EntityNotFoundException("Container not found with ID: " + containerId));

        // Fetch all assigned shipment ids
        shipmentsContainersMappings.addAll(shipmentsContainersMappingDao.findByContainerId(containerId));
        Set<Long> allAssignedShipmentIds = shipmentsContainersMappings.stream()
                .map(ShipmentsContainersMapping::getShipmentId)
                .collect(Collectors.toSet());

        // Fetch all assigned shipments
        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(allAssignedShipmentIds);
        shipmentDetailsMap.putAll(shipmentDetails.stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity())));
        allAssignedShipmentIds.forEach(id -> shipmentPackingMap.put(id, new ArrayList<>()));


        // Fetch all assigned packages
        List<Packing> packings = packingDao.findByShipmentIdInAndContainerId(allAssignedShipmentIds.stream().toList(), containerId);
        for (Packing packing : packings) {
            Long shipmentId = packing.getShipmentId();
            shipmentPackingMap
                    .computeIfAbsent(shipmentId, k -> new ArrayList<>())
                    .add(packing);
        }

        // assigning zero to weight and volume as it will be freshly recalculated
        containerV3Util.resetContainerDataForRecalculation(container);

        return container;
    }

    private List<Long> unAssignContainerCalculationsAndLogic(UnAssignContainerRequest request, Containers container, Map<Long,ShipmentDetails> shipmentDetailsMap,
                                                             Map<Long, List<Packing>> shipmentPackingMap, List<Long> shipmentIdsForCargoDetachment,
                                                             List<Long> removeAllPackingIds) throws RunnerException {
        List<Long> shipmentIdsForDetachment = new ArrayList<>();

        for (Map.Entry<Long, ShipmentDetails> entry : shipmentDetailsMap.entrySet()) {
            Long shipmentId = entry.getKey();
            ShipmentDetails shipmentDetails = entry.getValue();
            List<Packing> packingList = shipmentPackingMap.get(shipmentId);

            if (request.getShipmentPackIds().containsKey(shipmentId)) { // Shipment came for some/all packs detachment
                detachPacksAndShipmentFromContainer(request, container, packingList, shipmentIdsForCargoDetachment,
                        shipmentIdsForDetachment, removeAllPackingIds, shipmentDetails);
            } else { // Shipment and its packages remains intact i.e. not being detached
                addExistingShipmentAndPackagesToContainer(shipmentDetails, container, packingList);
            }
        }

        containerV3Util.setContainerNetWeight(container); // set container net weight from gross weight and tare weight
        return shipmentIdsForDetachment;
    }

    private void detachPacksAndShipmentFromContainer(UnAssignContainerRequest request, Containers container,
                                                     List<Packing> packingList, List<Long> shipmentIdsForCargoDetachment,
                                                     List<Long> shipmentIdsForDetachment, List<Long> removeAllPackingIds,
                                                     ShipmentDetails shipmentDetails) throws RunnerException {
        Long shipmentId = shipmentDetails.getId();
        Set<Long> removePackIds = new HashSet<>(request.getShipmentPackIds().get(shipmentId));

        // we are removing all the packages from this shipment, hence container will be detached from shipment
        if (Objects.equals(removePackIds.size(), packingList.size())) {
            shipmentIdsForDetachment.add(shipmentId);
            removeAllPackingIds.addAll(removePackIds);
            // check if we need to remove cargo link as well from shipment
            if (Objects.equals(shipmentDetails.getContainerAssignedToShipmentCargo(),
                    container.getId())) { // shipment cargo was linked to this container
                shipmentIdsForCargoDetachment.add(shipmentId);
            }
        } else { // only some packages are being removed from container
            removeAllPackingIds.addAll(removePackIds);
            // add data of remaining packages to container
            for (Packing packing : packingList) { // loop over all the assigned packs of shipment
                if (!removePackIds.contains(packing.getId())) { // this pack is not being detached
                    addPackageDataToContainer(container, packing);
                }
            }
        }
    }

    private void addExistingShipmentAndPackagesToContainer(ShipmentDetails shipmentDetails, Containers container, List<Packing> packingList) throws RunnerException {
        if (Objects.equals(shipmentDetails.getContainerAssignedToShipmentCargo(), container.getId())) { // container linked to this shipment Cargo
            addShipmentCargoToContainer(container, shipmentDetails);
        } else {
            for (Packing packing : packingList) { // loop over all the assigned packs of shipment
                if(Objects.equals(packing.getContainerId(), container.getId()))
                    addPackageDataToContainer(container, packing);
            }
        }
    }

    private Containers saveUnAssignContainerResults(List<Long> shipmentIdsForDetachment, List<Long> removeAllPackingIds,
                                                    List<Long> shipmentIdsForCargoDetachment, Containers container,
                                                    List<ShipmentsContainersMapping> shipmentsContainersMappings) {
        if (!listIsNullOrEmpty(shipmentIdsForCargoDetachment))
            shipmentDao.setShipmentIdsToContainer(shipmentIdsForCargoDetachment, null);
        if (!listIsNullOrEmpty(removeAllPackingIds))
            packingDao.setPackingIdsToContainer(removeAllPackingIds, null);
        container = containerDao.save(container);

        // detach shipment from containers
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>();
        for (ShipmentsContainersMapping shipmentsContainersMapping : shipmentsContainersMappings) {
            if (shipmentIdsForDetachment.contains(shipmentsContainersMapping.getShipmentId())) {
                shipmentsContainersMappingList.add(shipmentsContainersMapping);
            }
        }
        if (!listIsNullOrEmpty(shipmentsContainersMappingList))
            shipmentsContainersMappingDao.deleteAll(shipmentsContainersMappingList);
        return container;
    }

    @Override
    public void updateAttachedContainersData(List<Long> containerIds) throws RunnerException {
        if(listIsNullOrEmpty(containerIds)) {
            return;
        }
        ListCommonRequest listCommonRequest = constructListCommonRequest("containerAssignedToShipmentCargo", containerIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentDetailsList = shipmentDao.findAll(pair.getLeft(), pair.getRight());
        List<Packing> packingList = packingDao.findByContainerIdIn(containerIds);
        List<Containers> containers = findByIdIn(containerIds);
        Map<Long, Containers> containersMap = containers.stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity()));
        for(Containers containers1: containers) {
            containerV3Util.resetContainerDataForRecalculation(containers1);
        }
        for(ShipmentDetails shipmentDetails: shipmentDetailsList.getContent()) {
            addShipmentCargoToContainer(containersMap.get(shipmentDetails.getContainerAssignedToShipmentCargo()), shipmentDetails);
        }
        for(Packing packing: packingList) {
            addPackageDataToContainer(containersMap.get(packing.getContainerId()), packing);
        }
        for(Containers containers1: containers) {
            containerV3Util.setContainerNetWeight(containers1); // set container net weight from gross weight and tare weight
        }
        containerDao.saveAll(containers.stream().toList());
    }

}
