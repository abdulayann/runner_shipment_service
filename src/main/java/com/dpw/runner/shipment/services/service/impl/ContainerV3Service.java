package com.dpw.runner.shipment.services.service.impl;

import static com.dpw.runner.shipment.services.commons.constants.Constants.BOOKING;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CARGO_TYPE_LTL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_ID;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTAINER;
import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTAINER_INTERNAL_CALL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.OCEAN_DG_CONTAINER_FIELDS_VALIDATION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENTS_LIST;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_DRT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_TYPE_LCL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_RAI;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_ROA;
import static com.dpw.runner.shipment.services.commons.constants.Constants.TRANSPORT_MODE_SEA;
import static com.dpw.runner.shipment.services.commons.constants.ContainerConstants.CONTAINER_ALREADY_ASSIGNED_MSG;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.listIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.setIsNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.azure.messaging.servicebus.ServiceBusMessage;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.response.AttachedShipmentResponse;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerListResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerParams;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.AssignContainerRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ContainerBeforeSaveRequest;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerParams;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.UnAssignContainerRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.LoggerEvent;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
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
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IPackingV3Service;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentServiceV3;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.service_bus.model.ContainerBoomiUniversalJson;
import com.dpw.runner.shipment.services.service_bus.model.ContainerPayloadDetails;
import com.dpw.runner.shipment.services.service_bus.model.ContainerUpdateRequest;
import com.dpw.runner.shipment.services.service_bus.model.EventMessage;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ContainerV3Util;
import com.dpw.runner.shipment.services.utils.ContainerValidationUtil;
import com.dpw.runner.shipment.services.utils.FieldUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.v3.ConsolidationValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentValidationV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShippingInstructionUtil;
import com.nimbusds.jose.util.Pair;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.PostConstruct;
import javax.persistence.EntityNotFoundException;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;


@Service
@Slf4j
public class ContainerV3Service implements IContainerV3Service {

    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ISBUtils sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Value("${boomi-message-topic}")
    private String messageTopic;

    @Value("${containerTransportOrchestrator.queue}")
    private String transportOrchestratorQueue;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ContainerValidationUtil containerValidationUtil;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    IShipmentSync shipmentSync;

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
    @Lazy
    private ICustomerBookingV3Service customerBookingV3Service;

    @Autowired
    @Lazy
    private ContainerV3Service self;

    @Autowired
    private IPackingV3Service packingService;

    @Autowired
    @Lazy
    private IShipmentServiceV3 shipmentService;

    @Autowired
    @Lazy
    private IConsolidationV3Service consolidationV3Service;

    @Autowired
    private ConsolidationValidationV3Util consolidationValidationV3Util;

    @Autowired
    private ShipmentValidationV3Util shipmentValidationV3Util;

    @Autowired
    @Lazy
    private ShipmentsV3Util shipmentsV3Util;

    @Autowired
    private IShipmentsContainersMappingDao iShipmentsContainersMappingDao;

    @Autowired
    private ShippingInstructionUtil shippingInstructionUtil;

    private List<String> defaultIncludeColumns = new ArrayList<>();

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ContainerResponse create(ContainerV3Request containerRequest, String module) throws RunnerException {
        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting container creation | Request ID: {} | Request Body: {}", requestId, containerRequest);

        // Validate request parameters
        validateContainerRequest(containerRequest);
        updateContainerRequestOnDgFlag(List.of(containerRequest));

        // Process shipment-specific logic if module is SHIPMENT
        ShipmentDetails shipmentDetails = null;
        String consoleType = null;

        if (SHIPMENT.equalsIgnoreCase(module)) {
            shipmentDetails = processShipmentModule(List.of(containerRequest));
            consoleType = shipmentDetails.getJobType();
            if(Objects.equals(shipmentDetails.getMigrationStatus(), MigrationStatus.MIGRATED_FROM_V2)) {
                shipmentDetails.setTriggerMigrationWarning(false);
                shipmentDao.updateTriggerMigrationWarning(shipmentDetails.getId());
            }
        }
        containerValidationUtil.validateOpenForAttachment(containerRequest.getConsolidationId(), null);
        // Adjust allocation dates based on container number changes — sets current date if added/updated, clears if removed
        updateAllocationDateInRequest(List.of(containerRequest));

        // Validate container uniqueness
        List<Containers> containersList = getSiblingContainers(containerRequest, module, consoleType);
        containerValidationUtil.validateContainerNumberUniqueness(containerRequest.getContainerNumber(), containersList);

        // Convert DTO to Entity and perform pre-save operations
        Containers container = jsonHelper.convertValue(containerRequest, Containers.class);
        log.debug("Converted container request to entity | Entity: {}", container);

        ContainerBeforeSaveRequest containerBeforeSaveRequest = new ContainerBeforeSaveRequest();
        containerBeforeSave(containerRequest.getConsolidationId(), containerBeforeSaveRequest, List.of(containerRequest), module, shipmentDetails, true);

        // Save container to database
        Containers savedContainer = containerDao.save(container);
        log.info("Saved container entity to DB | Container ID: {} | Request ID: {}",
                savedContainer.getId(), requestId);

        // Handle post-save operations
        handlePostSaveOperations(savedContainer, containerRequest, containerBeforeSaveRequest, shipmentDetails, module);

        // Record audit logs
        recordAuditLogs(null, List.of(savedContainer), DBOperationType.CREATE);
        log.info("Audit log recorded for container creation | Container ID: {}", savedContainer.getId());

        // Convert and return response
        ContainerResponse response = jsonHelper.convertValue(savedContainer, ContainerResponse.class);
        log.info("Returning container response | Container ID: {} | Response: {}", savedContainer.getId(), response);

        return response;
    }

    private void validateContainerRequest(ContainerV3Request containerRequest) {
        boolean hasBookingId = containerRequest.getBookingId() != null;
        boolean hasConsolidationId = containerRequest.getConsolidationId() != null;
        boolean hasShipmentsId = containerRequest.getShipmentId() != null;

        if (!hasBookingId && !hasConsolidationId && !hasShipmentsId) {
            throw new ValidationException("Either BookingId, ConsolidationId, or ShipmentsId must be provided in the request.");
        }
    }

    private ShipmentDetails processShipmentModule(List<ContainerV3Request> containerV3RequestList) {
        ShipmentDetails shipmentDetails = shipmentDao.findById(containerV3RequestList.get(0).getShipmentId())
                .orElseThrow(() -> new ValidationException("Shipment not found for ID: " + containerV3RequestList.get(0).getShipmentId()));

        containerValidationUtil.validateShipmentForContainer(shipmentDetails);
        containerValidationUtil.validateShipmentCargoType(shipmentDetails);

        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentId(shipmentDetails.getId());
        if (!CommonUtils.listIsNullOrEmpty(consoleShipmentMappings)) {
            Long consolidationId = consoleShipmentMappings.get(0).getConsolidationId();
            for(ContainerV3Request containerV3Request : containerV3RequestList) containerV3Request.setConsolidationId(consolidationId);
        }else{
            throw new ValidationException("Shipment : " + shipmentDetails.getShipmentId() + ", must be attached to consolidation to Create container");
        }

        return shipmentDetails;
    }





    private void handlePostSaveOperations(Containers savedContainer, ContainerV3Request containerRequest,
                                          ContainerBeforeSaveRequest containerBeforeSaveRequest,
                                          ShipmentDetails shipmentDetails, String module) throws RunnerException {

        Optional.ofNullable(module)
                .filter(SHIPMENT::equals)
                .filter(m -> savedContainer.getId() != null)
                .filter(m -> containerRequest.getShipmentId() != null)
                .ifPresent(m -> shipmentsContainersMappingDao.assignShipments(
                        savedContainer.getId(),
                        Set.of(containerRequest.getShipmentId()),
                        false
                ));


        // Update shipment cargo details if module is SHIPMENT
        if (SHIPMENT.equalsIgnoreCase(module)) {
            List<Containers> shipmentContainers =  containerDao.findByShipmentId(containerRequest.getShipmentId());
            updateShipmentCargoDetails(shipmentDetails, new HashSet<>(shipmentContainers));
        }

        // Update consolidation cargo summary
        consolidationV3Service.updateConsolidationCargoSummary(containerBeforeSaveRequest.getConsolidationDetails(),
                containerBeforeSaveRequest.getShipmentWtVolResponse());

        // Handle other post-save actions
        handlePostSaveActions(savedContainer, containerRequest, module);
    }

    private void updateShipmentCargoDetails(ShipmentDetails shipmentDetails, Set<Containers> containersList) throws RunnerException {

        CargoDetailsResponse cargoDetailsResponse = shipmentService.calculateShipmentSummary(
                shipmentDetails.getTransportMode(),
                shipmentDetails.getPackingList(),
                containersList);

        if (cargoDetailsResponse != null) {
            shipmentService.updateCargoDetailsInShipment(shipmentDetails, cargoDetailsResponse);
        }
    }

    @Override
    public BulkContainerResponse createBulk(List<ContainerV3Request> containerRequests, String module) throws RunnerException {
        containerValidationUtil.validateCreateBulkRequest(containerRequests);
        containerValidationUtil.validateContainerNumberUniquenessForCreateBulk(containerRequests);
        String requestId = LoggerHelper.getRequestIdFromMDC();

        log.info("Starting container creation | Request ID: {} | Request Body: {}", requestId, containerRequests);

        // Convert DTO to Entity
        List<Containers> containers = jsonHelper.convertValueToList(containerRequests, Containers.class);
        log.debug("Converted container request to entity | Entity: {}", containers);

        // Save to DB
        List<Containers> savedContainers = containerDao.saveAll(containers);
        log.info("Saved containerList entity to DB | Request ID: {}", requestId);
        handlePostSaveActionsBulk(savedContainers, containerRequests, module);

        // Audit logging
        recordAuditLogs(null, savedContainers, DBOperationType.CREATE);
        log.info("Audit log recorded for container creation | Request ID: {}", requestId);

        List<ContainerResponse> containerResponses = jsonHelper.convertValueToList(containers, ContainerResponse.class);
        log.info("Returning containers response | Response: {}", containerResponses);
        return BulkContainerResponse.builder()
                .containerResponseList(containerResponses)
                .message(prepareBulkUpdateMessage(containerResponses))
                .build();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BulkContainerResponse updateBulk(List<ContainerV3Request> containerRequestList, String module) throws RunnerException {
        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting container UpdateBulk | Request ID: {} | Request Body: {}", requestId, containerRequestList);
        validateBulkUpdateRequest(containerRequestList);

        ShipmentDetails shipmentDetails = null;
        String consoleType = null;

        if (SHIPMENT.equalsIgnoreCase(module)) {
            shipmentDetails = processShipmentModule(containerRequestList);
            consoleType = shipmentDetails.getJobType();
            if(Objects.equals(shipmentDetails.getMigrationStatus(), MigrationStatus.MIGRATED_FROM_V2)) {
                shipmentDetails.setTriggerMigrationWarning(false);
                shipmentDao.updateTriggerMigrationWarning(shipmentDetails.getId());
            }
        }

        containerValidationUtil.validateOpenForAttachment(containerRequestList.get(0).getConsolidationId(), null);
        // Adjust allocation dates based on container number changes — sets current date if added/updated, clears if removed
        updateAllocationDateInRequest(containerRequestList);

        // Convert the request DTOs to entity models for persistence
        List<Containers> originalContainers = jsonHelper.convertValueToList(containerRequestList, Containers.class);
        log.debug("Converted updated container request to entity | Entity: {}", originalContainers);

        List<Containers> containersList = getSiblingContainers(containerRequestList.get(0), module, consoleType);

        boolean isAutoSell = false;

        Map<UUID, Containers> oldContainers = containersList.stream().collect(Collectors.toMap(Containers::getGuid, container->container,(container1, container2)->container1));

        for (Containers containers: originalContainers) {
            Containers containers1 = oldContainers.get(containers.getGuid());
            if (containers1 != null && (!Objects.equals(containers1.getContainerCount(), containers.getContainerCount()) || !Objects.equals(containers1.getContainerCode(), containers.getContainerCode()))) {
                isAutoSell = true;
                break;
            }
        }
        // before save operations
        ContainerBeforeSaveRequest containerBeforeSaveRequest = new ContainerBeforeSaveRequest();
        containerBeforeSave(containerRequestList.get(0).getConsolidationId(), containerBeforeSaveRequest, containerRequestList, module, shipmentDetails, false);

        for(ContainerV3Request containerRequest : containerRequestList){
            List<Containers> containers = new ArrayList<>(containersList);
            if(containerRequest.getId() != null) {
                containers.removeIf(container -> container.getId() != null && container.getId()
                        .equals(containerRequest.getId()));
            }

            containerValidationUtil.validateContainerNumberUniqueness(containerRequest.getContainerNumber(), containers);
        }

        // Save the updated containers to the database
        List<Containers> updatedContainers = containerDao.saveAll(originalContainers);

        // Update shipment cargo details if module is SHIPMENT
        if (SHIPMENT.equalsIgnoreCase(module)) {
            List<Containers> shipmentContainers =  containerDao.findByShipmentId(containerRequestList.get(0).getShipmentId());
            updateShipmentCargoDetails(shipmentDetails, new HashSet<>(shipmentContainers));
        }

        // update console achieved data
        consolidationV3Service.updateConsolidationCargoSummary(containerBeforeSaveRequest.getConsolidationDetails(),
                containerBeforeSaveRequest.getShipmentWtVolResponse());

        runAsyncPostSaveOperations(updatedContainers,isAutoSell, module);

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

    private void validateBulkUpdateRequest(List<ContainerV3Request> containerRequestList) {
        updateContainerRequestOnDgFlag(containerRequestList);
        containerValidationUtil.validateUpdateBulkRequest(containerRequestList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BulkContainerResponse deleteBulk(List<ContainerV3Request> containerRequestList, String module) throws RunnerException {
        return self.deleteBulk(containerRequestList, module, false);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BulkContainerResponse deleteBulk(List<ContainerV3Request> containerRequestList, String module, boolean isForceDelete) throws RunnerException {
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

        // Validate the open for attachment flag of all the container's consols
        containerValidationUtil.validateOpenForAttachment(containersToDelete);

        // Validate that the containers are not assigned to any active shipment or packing before deletion
        if(!isForceDelete)
            validateNoAssignments(containerIds, module);
        else {
            List<ShipmentsContainersMapping> shipmentsContainersMappingList = shipmentsContainersMappingDao.findByContainerIdIn(containerIds);
            if(!listIsNullOrEmpty(shipmentsContainersMappingList)) {
                List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentsContainersMappingList.stream().map(ShipmentsContainersMapping::getShipmentId).collect(Collectors.toSet()));
                // Validate shipment type for container operation
                containerValidationUtil.validateShipmentTypeForContainerOperation(containersToDelete, shipmentDetails, shipmentsContainersMappingList, module);
                List<Packing> packings = packingDao.findByContainerIdIn(containerIds);
                bulkUnAssign(shipmentDetails, containerIds, packings, Boolean.FALSE, Boolean.TRUE, Boolean.TRUE);
            }
        }

        // Collect all unique shipment IDs that are associated with the containers to delete
        List<Long> shipmentIds = containersToDelete.stream().map(Containers::getShipmentsList)
                .filter(Objects::nonNull)
                .flatMap(Set::stream).map(ShipmentDetails::getId).distinct().toList();

        Long consolidationId = containersToDelete.get(0).getConsolidationId();
        ContainerBeforeSaveRequest containerBeforeSaveRequest = new ContainerBeforeSaveRequest();
        getConsoleAchievedDataBefore(consolidationId, containerBeforeSaveRequest);

        // Proceed with the deletion of the containers and any related associations (shipment, packing, etc.)
        deleteContainerAndAssociations(containerIds,  containersToDelete);

        if (SHIPMENT.equalsIgnoreCase(module)) {
            // Shipment container only allowed to delete from shipment i.e shipmentIds size will always be 1
            Long shipmentId = shipmentIds.get(0);
            ShipmentDetails shipmentDetails = shipmentDao.findById(shipmentId)
                    .orElseThrow(() -> new ValidationException("Shipment is not present with ID : " + shipmentId));
            List<Containers> shipmentContainers = containerDao.findByShipmentId(shipmentId);

            updateShipmentCargoDetails(shipmentDetails, new HashSet<>(shipmentContainers));
            if(Objects.equals(shipmentDetails.getMigrationStatus(), MigrationStatus.MIGRATED_FROM_V2)) {
                shipmentDetails.setTriggerMigrationWarning(false);
                shipmentDao.updateTriggerMigrationWarning(shipmentId);
            }
        }

        consolidationV3Service.updateConsolidationCargoSummary(containerBeforeSaveRequest.getConsolidationDetails(),
                containerBeforeSaveRequest.getShipmentWtVolResponse());

        if (Objects.equals(BOOKING, module)) {
            customerBookingV3Service.updateContainerInfoInBooking(containersToDelete.iterator().next().getBookingId());
        }

        // Log the deletion activity for auditing and tracking purposes
        recordAuditLogs(containersToDelete, null, DBOperationType.DELETE);

        // Return a response indicating the result of the bulk delete operation
        return BulkContainerResponse.builder()
                .message(prepareBulkDeleteMessage(containersToDelete))
                .build();
    }

    @Override
    public void bulkUnAssign(List<ShipmentDetails> shipmentDetails, List<Long> containerIds, List<Packing> shipmentPackings,
                             boolean isFCLDelete, boolean isForcedDetach, boolean fromDelete) throws RunnerException {
        // Process each container using the internal methods of unAssignContainers one by one
        // Collect all results for batch processing
        Map<String, List<Containers>> containersToSaveMap = new HashMap<>();
        List<List<Long>> allShipmentIdsForDetachment = new ArrayList<>();
        List<UnAssignContainerParams> unAssignContainerParamsList =  new ArrayList<>();

        for (Long containerId : containerIds) {
            // Create UnAssignContainerRequest for this container
            UnAssignContainerRequest unAssignRequest = new UnAssignContainerRequest();
            unAssignRequest.setContainerId(containerId);

            // Building shipmentPackIds → Map<shipmentId, List<packingIds>>
            Map<Long, List<Long>> shipmentPackIds = getShipmentPackIds(shipmentDetails, containerId, shipmentPackings);

            unAssignRequest.setShipmentPackIds(shipmentPackIds);
            // Step 2: Create UnAssignContainerParams for this operation
            UnAssignContainerParams unAssignContainerParams = new UnAssignContainerParams();
            self.unAssignContainers(unAssignRequest, Constants.CONSOLIDATION_PACKING, unAssignContainerParams, containersToSaveMap,
                                    allShipmentIdsForDetachment, unAssignContainerParamsList, false, isForcedDetach);
        }
        self.saveUnAssignContainerResultsBatch(allShipmentIdsForDetachment, containersToSaveMap, unAssignContainerParamsList, isFCLDelete, fromDelete);
        updateShipmentSummary(unAssignContainerParamsList);
    }

    private static Map<Long, List<Long>> getShipmentPackIds(List<ShipmentDetails> shipmentDetails, Long containerId, List<Packing> shipmentPackings) {
        Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
        for (ShipmentDetails ship : shipmentDetails) {
            List<Long> shipContIds = ship.getContainersList().stream()
                    .filter(container -> container != null && container.getId() != null)
                    .map(Containers::getId).toList();
            if (shipContIds.contains(containerId)) {
                shipmentPackIds.put(ship.getId(), new ArrayList<>());
            }
        }
        for (Packing packing : shipmentPackings) {
            if (containerId.equals(packing.getContainerId())) {
                Long shipmentId = packing.getShipmentId();
                if (shipmentPackIds.containsKey(shipmentId)) {
                    shipmentPackIds.get(shipmentId).add(packing.getId());
                }
            }
        }
        return shipmentPackIds;
    }

    public void updateShipmentSummary(List<UnAssignContainerParams> unAssignContainerParamsList) throws RunnerException {
        if (unAssignContainerParamsList == null || unAssignContainerParamsList.isEmpty()) {
            return;
        }
        for(UnAssignContainerParams unAssignContainerParams: unAssignContainerParamsList) {
            Set<Long> shipmentIds = unAssignContainerParams.getFclOrFtlShipmentIds();
            Map<Long, ShipmentDetails> shipmentDetailsMap = unAssignContainerParams.getShipmentDetailsMap();

            if (setIsNullOrEmpty(shipmentIds) || shipmentDetailsMap == null || shipmentDetailsMap.isEmpty()) {
                continue;
            }

            for (Long shipmentId : shipmentIds) {

                ShipmentDetails shipmentDetails = shipmentDetailsMap.get(shipmentId);
                if (shipmentDetails != null) {
                    shipmentService.calculateAndUpdateShipmentCargoSummary(shipmentDetails);
                }
            }
        }
    }

    private void containerBeforeSave(Long consolidationId, ContainerBeforeSaveRequest containerBeforeSaveRequest,
                                     List<ContainerV3Request> containerRequestList, String module, ShipmentDetails shipmentDetails, boolean isCreate) throws RunnerException {
        processContainerDG(containerRequestList, module, shipmentDetails, isCreate);
        getConsoleAchievedDataBefore(consolidationId, containerBeforeSaveRequest);
    }

    private void updateAllocationDateInRequest(List<ContainerV3Request> containerRequestList) {
        try {
            if (containerRequestList == null || containerRequestList.isEmpty()) {
                // No containers provided, nothing to update
                return;
            }

            // Separate requests into "new" containers (no ID) and "existing" containers (with ID)
            List<ContainerV3Request> existingContainerRequests = containerRequestList.stream()
                    .filter(request -> request.getId() != null)
                    .toList();

            List<ContainerV3Request> newContainerRequests = containerRequestList.stream()
                    .filter(request -> request.getId() == null)
                    .toList();

            // Fetch all existing containers from DB for comparison
            List<Long> existingContainerIds = existingContainerRequests.stream()
                    .map(ContainerV3Request::getId).filter(Objects::nonNull).toList();

            List<Containers> existingContainersFromDb = existingContainerIds.isEmpty()
                    ? Collections.emptyList()
                    : containerDao.findByIdIn(existingContainerIds);

            // Map of containerId → Containers (from DB)
            Map<Long, Containers> containerIdToEntityMap = existingContainersFromDb.stream()
                    .filter(Objects::nonNull)
                    .collect(Collectors.toMap(Containers::getId, Function.identity()));

            // CASE 1: New containers → Assign allocation date if container number present
            for (ContainerV3Request newRequest : newContainerRequests) {
                if (newRequest.getContainerNumber() != null) {
                    newRequest.setAllocationDate(LocalDateTime.now());
                }
            }

            // CASE 2: Existing containers → Compare old and new container numbers
            for (ContainerV3Request request : existingContainerRequests) {
                Containers existingEntity = containerIdToEntityMap.get(request.getId());

                // If container not found in DB, skip
                if (existingEntity == null) {
                    continue;
                }

                String oldContainerNumber = existingEntity.getContainerNumber();
                String newContainerNumber = request.getContainerNumber();

                if (oldContainerNumber != null && newContainerNumber == null) {
                    // Container number deleted → remove allocation date
                    request.setAllocationDate(null);
                } else if (!Objects.equals(oldContainerNumber, newContainerNumber)) {
                    // Container number added or updated → set allocation date to now
                    request.setAllocationDate(LocalDateTime.now());
                } else {
                    // No change → retain the existing allocation date from DB
                    request.setAllocationDate(existingEntity.getAllocationDate());
                }
            }
        } catch (Exception ex) {
            throw new ValidationException("Error occurred while updating allocation dates for containers", ex);
        }
    }

    protected void processContainerDG(List<ContainerV3Request> containerRequestList, String module, ShipmentDetails shipmentDetails, boolean isCreate) throws RunnerException {
        if (!Set.of(SHIPMENT, CONSOLIDATION).contains(module)) return;
        if(!containsHazardousContainer(containerRequestList)) return;
        if (SHIPMENT.equalsIgnoreCase(module)) {
            List<Containers> oldShipmentContainers = containerDao.findByShipmentId(containerRequestList.get(0).getShipmentId());
            validateAndSaveDGShipment(oldShipmentContainers, shipmentDetails, containerRequestList, isCreate);
        } else {
                Long consolidationId = containerRequestList.get(0).getConsolidationId();
                ConsolidationDetails consolidationDetails = consolidationV3Service.fetchConsolidationDetails(consolidationId);
                validateAndProcessDGConsolidation(containerRequestList, consolidationDetails);

        }
    }

    private void validateAndProcessDGConsolidation(List<ContainerV3Request> containerRequestList,
                                                   ConsolidationDetails consolidationDetails) throws RunnerException {
        if (TRANSPORT_MODE_SEA.equalsIgnoreCase(consolidationDetails.getTransportMode())) {
            consolidationDetails.setHazardous(true);
            if (!consolidationValidationV3Util.checkConsolidationTypeValidation(consolidationDetails)) {
                throw new ValidationException("For Ocean LCL DG Consolidation, the Console type can only be AGT or CLD");
            }
            consolidationDetailsDao.update(consolidationDetails, false, false);
            processDGShipmentDetailsFromContainer(containerRequestList);
        }
    }

    public void processDGShipmentDetailsFromContainer(ContainerV3Request containerV3Request) throws RunnerException {
        if (containerV3Request.getId() != null) {
            List<ShipmentsContainersMapping> shipmentsContainersMappingList = iShipmentsContainersMappingDao.findByContainerId(
                    containerV3Request.getId());
            for (ShipmentsContainersMapping shipmentsContainersMapping : shipmentsContainersMappingList) {
                Long shipmentId = shipmentsContainersMapping.getId();
                Optional<ShipmentDetails> optionalShipmentDetails = shipmentService.findById(
                        shipmentId);
                if (optionalShipmentDetails.isPresent()) {
                    ShipmentDetails shipmentDetails = optionalShipmentDetails.get();
                    shipmentDetails.setContainsHazardous(true);
                    shipmentValidationV3Util.processDGValidations(shipmentDetails, null, shipmentDetails.getConsolidationList());
                    callChangeShipmentDGStatusFromContainer(shipmentDetails, containerV3Request);
                    shipmentDao.save(shipmentDetails, false, false);
                }
            }
        }
    }

    public void processDGShipmentDetailsFromContainer(List<ContainerV3Request> containerRequestList) throws RunnerException {
        for(ContainerV3Request containerV3Request : containerRequestList) {
            if (containerV3Request.getId() != null && Boolean.TRUE.equals(containerV3Request.getHazardous())) {
                List<ShipmentsContainersMapping> shipmentsContainersMappingList = iShipmentsContainersMappingDao.findByContainerId(containerV3Request.getId());

                for (ShipmentsContainersMapping shipmentsContainersMapping : shipmentsContainersMappingList) {
                    Long shipmentId = shipmentsContainersMapping.getShipmentId();
                    Optional<ShipmentDetails> optionalShipmentDetails = shipmentService.findById(shipmentId);
                    if (optionalShipmentDetails.isPresent()) {
                        ShipmentDetails shipmentDetails = optionalShipmentDetails.get();
                        List<Containers> containersList = containerDao.findByShipmentId(shipmentId);
                        updateOceanDgStatusForCreateUpdate(shipmentDetails, containersList, containerRequestList, false);
                    }
                }
            }
        }
    }

    private boolean isUpdateDGStatusRequired(ShipmentDetails shipmentDetails, List<Containers> containersList) {
        if (shipmentDetails == null) return false;
        if (CommonUtils.listIsNullOrEmpty(containersList)) return false;
        return TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode());
    }
    private boolean isUpdateDGStatusRequired1(ShipmentDetails shipmentDetails, List<ContainerV3Request> containersList) {
        if (shipmentDetails == null) return false;
        if (CommonUtils.listIsNullOrEmpty(containersList)) return false;
        return TRANSPORT_MODE_SEA.equals(shipmentDetails.getTransportMode());
    }
    protected void updateStatusForOceanDG(ShipmentDetails shipmentDetails, List<Containers> containersList, List<ContainerV3Request> containerRequestList) throws RunnerException {
        if (!isUpdateDGStatusRequired(shipmentDetails, containersList)) {
            return;
        }

        Set<Long> containerIds = containersList.stream().map(Containers::getId).collect(Collectors.toSet());
        Set<Long> requestIds = containerRequestList.stream()
                .map(ContainerV3Request::getId)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        Map<Long, ContainerV3Request> updatedContainerRequestMap = containerRequestList.stream()
                .filter(Objects::nonNull)
                .filter(container -> container.getId() != null)
                .collect(Collectors.toMap(
                        ContainerV3Request::getId,
                        Function.identity()
                ));

        Map<Long, Containers> oldContainerMap = containersList.stream()
                .filter(Objects::nonNull)
                .filter(container -> container.getId() != null)
                .collect(Collectors.toMap(
                        Containers::getId,
                        Function.identity()
                ));
        Set<Long> commonIds = new HashSet<>(containerIds);
        commonIds.retainAll(requestIds); // intersection

        boolean isDG = false;
        boolean isDGClass1Added = false;

        for(Long containerId : commonIds){
            Containers oldContainer = oldContainerMap.get(containerId);
            ContainerV3Request updatedContainer = updatedContainerRequestMap.get(containerId);
            if(commonUtils.checkIfDGFieldsChangedInContainer(updatedContainer, oldContainer)){
                isDGClass1Added = isDGClass1Added || commonUtils.checkIfDGClass1(updatedContainer.getDgClass());
                isDG = true;
            }
        }
        if(isDG){
            boolean saveShipment = commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1Added);
            if(saveShipment) {
                shipmentDetails.setContainsHazardous(true);
                shipmentValidationV3Util.processDGValidations(shipmentDetails, null, shipmentDetails.getConsolidationList());
                String oceanDGStatus = shipmentDetails.getOceanDGStatus() != null ? shipmentDetails.getOceanDGStatus().name() : null;
                shipmentDao.updateDgStatusInShipment(true, oceanDGStatus, shipmentDetails.getId());
            }
        }
    }

    public void validateAndSaveDGShipment(List<Containers> shipmentContainers, ShipmentDetails shipmentDetails, List<ContainerV3Request> containerRequestList, boolean isCreate) throws RunnerException {
        updateOceanDgStatusForCreateUpdate(shipmentDetails, shipmentContainers, containerRequestList, isCreate);
    }
    public boolean containsHazardousContainer(List<ContainerV3Request> containerRequestList) {
        return containerRequestList.stream()
                .anyMatch(request -> Boolean.TRUE.equals(request.getHazardous()));
    }
    private void updateOceanDGStatusCreate(ShipmentDetails shipmentDetails, List<ContainerV3Request> containerRequestList) throws RunnerException {
        boolean isDG = false;
        boolean isDGClass1Added = false;
        for(ContainerV3Request containerV3Request : containerRequestList){
            if(Boolean.TRUE.equals(containerV3Request.getHazardous())){
                isDG = true;
                isDGClass1Added = isDGClass1Added || commonUtils.checkIfDGClass1(containerV3Request.getDgClass());
            }
        }

        if(isDG){
            saveOceanDgStatus(shipmentDetails, isDGClass1Added);
        }
    }

    private void updateOceanDGStatusUpdate(ShipmentDetails shipmentDetails, List<Containers> oldContainers, List<ContainerV3Request> containerRequestList) throws RunnerException {
        Set<Long> containerIds = oldContainers.stream().map(Containers::getId).collect(Collectors.toSet());
        Set<Long> requestIds = containerRequestList.stream().map(ContainerV3Request::getId).filter(Objects::nonNull).collect(Collectors.toSet());
        Map<Long, ContainerV3Request> updatedContainerRequestMap = containerRequestList.stream()
                .filter(Objects::nonNull)
                .filter(container -> container.getId() != null)
                .collect(Collectors.toMap(ContainerV3Request::getId, Function.identity()));
        Map<Long, Containers> oldContainerMap = oldContainers.stream()
                .filter(Objects::nonNull)
                .filter(container -> container.getId() != null)
                .collect(Collectors.toMap(Containers::getId, Function.identity()));
        Set<Long> commonIds = new HashSet<>(containerIds);
        commonIds.retainAll(requestIds); // intersection
        boolean isDG = false;
        boolean isDGClass1Added = false;
        for(Long containerId : commonIds){
            Containers oldContainer = oldContainerMap.get(containerId);
            ContainerV3Request updatedContainer = updatedContainerRequestMap.get(containerId);
            if(commonUtils.checkIfDGFieldsChangedInContainer(updatedContainer, oldContainer)){
                isDGClass1Added = isDGClass1Added || commonUtils.checkIfDGClass1(updatedContainer.getDgClass());
                isDG = true;
            }
        }
        if(isDG){
            saveOceanDgStatus(shipmentDetails, isDGClass1Added);
        }
    }
    protected void updateOceanDgStatusForCreateUpdate(ShipmentDetails shipmentDetails, List<Containers> oldContainers
            , List<ContainerV3Request> containerRequestList, boolean isCreate) throws RunnerException {
        if(!isUpdateDGStatusRequired1(shipmentDetails, containerRequestList)) return;
        if(isCreate){
            updateOceanDGStatusCreate(shipmentDetails, containerRequestList);
        }else{
            updateOceanDGStatusUpdate(shipmentDetails, oldContainers, containerRequestList);
        }
    }
    private void saveOceanDgStatus(ShipmentDetails shipmentDetails, boolean isDGClass1Added) throws RunnerException {
        boolean saveShipment = commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1Added);
        if(saveShipment) {
            shipmentDetails.setContainsHazardous(true);
            shipmentValidationV3Util.processDGValidations(shipmentDetails, null, shipmentDetails.getConsolidationList());
            String oceanDGStatus = shipmentDetails.getOceanDGStatus() != null ? shipmentDetails.getOceanDGStatus().name() : null;
            shipmentDao.updateDgStatusInShipment(true, oceanDGStatus, shipmentDetails.getId());
        }
    }

    public void callChangeShipmentDGStatusFromContainer(ShipmentDetails shipmentDetails, ContainerV3Request container) {
        Containers oldContainer = null;
        boolean isDGClass1 = commonUtils.checkIfDGClass1(container.getDgClass());
        if(container.getId() == null) {
            commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
        }
        if(container.getId() != null) {
               oldContainer = containerRepository.getById(container.getId());
            if(commonUtils.checkIfDGFieldsChangedInContainer(container, oldContainer)) {
                commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1);
            }
        }
    }



    private void getConsoleAchievedDataBefore(Long consolidationId, ContainerBeforeSaveRequest containerBeforeSaveRequest) throws RunnerException {
        if(consolidationId != null) {
            ConsolidationDetails consolidationDetails = consolidationV3Service.fetchConsolidationDetails(consolidationId);
            containerBeforeSaveRequest.setConsolidationDetails(consolidationDetails);
            containerBeforeSaveRequest.setShipmentWtVolResponse(consolidationV3Service.calculateShipmentWtVol(consolidationDetails));
        }
    }

    /**
     * Validates that the given containers are not assigned to any packages or shipment cargo before allowing deletion.
     *
     * <p>The method performs the following checks in order:</p>
     * <ol>
     *     <li>If any containers are assigned to both Packages and Shipment Cargo — it merges them without duplicates
     *         and throws an exception showing the details.</li>
     *     <li>If containers are only assigned to Packages — it throws an exception with package assignment details.</li>
     *     <li>If containers are only assigned to Shipment Cargo — it throws an exception with cargo assignment details.</li>
     * </ol>
     *
     * @param containerIds list of container IDs to be validated for deletion
     * @throws IllegalArgumentException if any container is found to be assigned and cannot be deleted
     */
    private void validateNoAssignments(List<Long> containerIds, String module) {
        // Fetch containers that are assigned to packages
        List<ContainerDeleteInfoProjection> packingOnly = containerDao.filterContainerIdsAttachedToPacking(containerIds);

        boolean hasPacking = ObjectUtils.isNotEmpty(packingOnly);

        if(CONSOLIDATION.equalsIgnoreCase(module)) {
            // Fetch containers that are assigned to shipment cargo
        List<ContainerDeleteInfoProjection> shipmentCargoOnly = containerDao.filterContainerIdsAttachedToShipmentCargo(containerIds);

        // Fetch containers that are assigned to shipment
        List<ContainerDeleteInfoProjection> shipmentOnly = containerDao.filterContainerIdsAttachedToShipment(containerIds);

        boolean hasShipmentCargoOnly = ObjectUtils.isNotEmpty(shipmentCargoOnly);
        boolean hasShipmentOnly = ObjectUtils.isNotEmpty(shipmentOnly);
        // If containers are assigned to both packing and shipment cargo
        if (hasPacking && hasShipmentCargoOnly) {
            // Merge both lists while removing duplicates based on containerId
            List<ContainerDeleteInfoProjection> merged = Stream.concat(packingOnly.stream(), shipmentCargoOnly.stream())
                    .collect(Collectors.toMap(
                            ContainerDeleteInfoProjection::getContainerId, // Use containerId as the unique key
                            p -> p, // Keep the projection object as-is
                            (p1, p2) -> p1 // If duplicate keys, prefer the one from packingOnly
                    )).values().stream().toList();

            // Throw an exception with the combined message
            throw new IllegalArgumentException(
                    "Selected containers are assigned to both Shipment Cargo and Packages. Please unassign them before deletion:\n" +
                            formatAssignedContainersInfo(merged)
            );
        }

            // If containers are assigned only to shipment cargo
            if (hasShipmentCargoOnly) {
                throw new IllegalArgumentException(
                        "Selected containers are assigned to Shipment Cargo. Please unassign them before deletion:\n" +
                                formatAssignedContainersInfo(shipmentCargoOnly)
                );
            }

            // If containers are assigned only to shipment
            if (hasShipmentOnly) {
                throw new IllegalArgumentException(
                        "Selected containers are assigned to Shipment. Please unassign them before deletion:\n" +
                                formatAssignedContainersInfo(shipmentOnly)
                );
            }
        }

        // If containers are assigned only to packages
        if (hasPacking) {
            throw new IllegalArgumentException(
                    "Selected containers are assigned to Packages. Please unassign them before deletion:\n" +
                            formatAssignedContainersInfo(ObjectUtils.isNotEmpty(packingOnly) ? List.of(packingOnly.get(0)) : List.of())
            );
        }

    }

    // Formats the display information for containers with optional package details
    private String formatAssignedContainersInfo(List<ContainerDeleteInfoProjection> projections) {
        return projections.stream()
                .map(p -> {
                    String containerNumber = Optional.ofNullable(p.getContainerNumber()).orElse(p.getContainerCode());
                    String shipmentId = p.getShipmentId();

                    // Only include packages info if packs is not null/empty
                    String packagesInfo = Optional.ofNullable(p.getPacks())
                            .filter(packs -> !packs.trim().isEmpty())
                            .map(packs -> String.format(" - Packages: %s %s", packs, Optional.ofNullable(p.getPacksType()).orElse("")))
                            .orElse("");

                    return String.format("Container Number: %s - Shipment Number: %s%s", containerNumber, shipmentId, packagesInfo);
                }).collect(Collectors.joining("\n"));
    }

    // Method to handle the deletion of containers and their associated entities
    private void deleteContainerAndAssociations(List<Long> containerIds, List<Containers> containersToDelete) {
        log.info("Starting deleteContainerAndAssociations with containerIds: {}", containerIds);
        for(Containers containers : containersToDelete) containers.setShipmentsList(new HashSet<>());
        containerDao.saveAll(containersToDelete);

        containerDao.deleteByIdIn(containerIds);
        log.info("Successfully deleted {} containers.", containerIds.size());
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

    protected List<Containers> getSiblingContainers(ContainerV3Request containerRequest, String module, String consoleType) {
        if (SHIPMENT.equalsIgnoreCase(module)) {
            if (SHIPMENT_TYPE_DRT.equalsIgnoreCase(consoleType)) {
                return containerDao.findByShipmentId(containerRequest.getShipmentId());
            }
            return containerDao.findByConsolidationId(containerRequest.getConsolidationId());
        }
        if (Objects.nonNull(containerRequest.getConsolidationId())) {
            return containerDao.findByConsolidationId(containerRequest.getConsolidationId());
        }

        if (Objects.nonNull(containerRequest.getBookingId())) {
            return containerDao.findByBookingIdIn(List.of(containerRequest.getBookingId()));
        }

        return Collections.emptyList();
    }

    /**
     * Post-processing logic invoked after saving a container entity.
     * <p>
     * Ensures the tenant ID is set on the container if missing,
     * then constructs and pushes a Kafka message to the configured internal queue.
     * Any failures during Kafka operations are logged.
     * </p>
     *
     * @param container             the container entity that was persisted
     * @param isAutoSell
     * @param isCreate              true if the container was newly created; false if updated
     */
    private void afterSave(Containers container, boolean isAutoSell, boolean isCreate) {
        try {
            // Set tenant from context if not already present on the container
            if (container.getTenantId() == null) {
                container.setTenantId(TenantContext.getCurrentTenant());
            }
            triggerPushToDownStream(container, isAutoSell, isCreate, Constants.CONTAINER_AFTER_SAVE);

        } catch (Exception ex) {
            // Log failure with detailed error message and exception stack trace
            log.error("Failed to push container update to Kafka | containerId={} | error={}",
                    container.getId(), ex.getMessage(), ex);
        }
    }

    private void triggerPushToDownStream(Containers container, Boolean isAutoSell, boolean isCreate, String sourceInfo) {
        String transactionId = UUID.randomUUID().toString();

        log.info("[InternalKafkaPush] Initiating downstream internal Kafka push | containerId={} | isCreate={} | transactionId={}",
                container.getId(), isCreate, transactionId);
        List<PushToDownstreamEventDto.Triggers> triggersList = new ArrayList<>();
        PushToDownstreamEventDto pushToDownstreamEventDto = PushToDownstreamEventDto.builder()
                .parentEntityId(container.getId())
                .parentEntityName(CONTAINER)
                .meta(PushToDownstreamEventDto.Meta.builder()
                        .sourceInfo(sourceInfo)
                        .isAutoSellRequired(isAutoSell)
                        .tenantId(container.getTenantId())
                        .isCreate(isCreate).build()).build();

        if (container.getConsolidationId() != null) {
            PushToDownstreamEventDto.Triggers vTrigger = PushToDownstreamEventDto.Triggers.builder()
                    .entityId(container.getConsolidationId())
                    .entityName(Constants.CONSOLIDATION)
                    .build();
            triggersList.add(vTrigger);
        }

        if (ObjectUtils.isNotEmpty(container.getShipmentsList())) {
            for (ShipmentDetails details : container.getShipmentsList()) {
                PushToDownstreamEventDto.Triggers vTrigger = PushToDownstreamEventDto.Triggers.builder()
                        .entityId(details.getId())
                        .entityName(Constants.SHIPMENT).build();
                triggersList.add(vTrigger);
            }
        }
        if (ObjectUtils.isNotEmpty(container.getBookingId())) {
            PushToDownstreamEventDto.Triggers vTrigger = PushToDownstreamEventDto.Triggers.builder()
                    .entityId(container.getBookingId())
                    .entityName(Constants.CUSTOMER_BOOKING).build();
            triggersList.add(vTrigger);
        }

        pushToDownstreamEventDto.setTriggers(triggersList);
        dependentServiceHelper.pushToKafkaForDownStream(pushToDownstreamEventDto, transactionId);

        log.info("[InternalKafkaPush] Message successfully pushed to internal Kafka | containerId={} | transactionId={}",
                container.getId(), transactionId);
    }

    @Override
    public ContainerSummaryResponse calculateContainerSummary(Long shipmentId, Long consolidationId, String xSource) throws RunnerException {
        if (shipmentId == null && consolidationId == null) {
            throw new RunnerException("Please provide shipmentId and consolidationId for containers summary");
        }
        boolean canFetchDetailsWithoutTenantCheck = CommonUtils.canFetchDetailsWithoutTenantFilter(xSource);
        if (shipmentId != null) {
            List<Containers> containers;
            if (canFetchDetailsWithoutTenantCheck)
                containers = containerDao.findByShipmentIdWithoutTenantFilter(shipmentId);
            else
                containers = containerDao.findByShipmentId(shipmentId);
            List<Containers> containersList = new ArrayList<>(containers);
            Optional<ShipmentDetails> shipmentDetailsEntity = shipmentService.findById(shipmentId);
            List<ContainerBaseResponse> containerBaseResponseList = jsonHelper.convertValueToList(containersList, ContainerBaseResponse.class);
            if (shipmentDetailsEntity.isPresent()) {
                ShipmentDetails shipmentDetails = shipmentDetailsEntity.get();
                if(List.of(TRANSPORT_MODE_SEA, TRANSPORT_MODE_RAI, TRANSPORT_MODE_ROA).contains(shipmentDetails.getTransportMode()) && List.of(SHIPMENT_TYPE_LCL, CARGO_TYPE_LTL).contains(shipmentDetails.getShipmentType())) {
                    containerV3Util.updatedContainerResponseForLCLandLTL(containerBaseResponseList, shipmentDetails);
                }
            }
            return getContainerSummaryResponse(jsonHelper.convertValueToList(containerBaseResponseList, Containers.class), true, xSource);
        }
        List<Containers> containers;
        if (canFetchDetailsWithoutTenantCheck)
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

        Optional<ShipmentDetails> shipmentDetailsEntity = shipmentService.findById(Long.valueOf(request.getEntityId()));
        if (shipmentDetailsEntity.isPresent()) {
            ShipmentDetails shipmentDetails = shipmentDetailsEntity.get();
            if(List.of(TRANSPORT_MODE_SEA, TRANSPORT_MODE_RAI, TRANSPORT_MODE_ROA).contains(shipmentDetails.getTransportMode()) && List.of(SHIPMENT_TYPE_LCL, CARGO_TYPE_LTL).contains(shipmentDetails.getShipmentType())) {
                containerV3Util.updatedContainerResponseForLCLandLTL(containerListResponse.getContainers(), shipmentDetails);
            }
            containerListResponse.setTriggerMigrationWarning(shipmentDetails.getTriggerMigrationWarning());
        }

        return containerListResponse;

    }

    @Override
    public ContainerListResponse fetchConsolidationContainers(ListCommonRequest request, String xSource) throws RunnerException {
        ListCommonRequest enrichedRequest = getEnrichedRequest(request);

        ContainerListResponse containerListResponse;
        try {
            containerListResponse = list(enrichedRequest, true, xSource);
        } catch (Exception ex) {
            throw new IllegalArgumentException("Failed to fetch consolidation containers", ex);
        }

        return processAfterList(containerListResponse);
    }

    private void setAssignedContainer(ContainerListResponse containerListResponse, String xSource) {
        List<Long> containersId = containerListResponse.getContainers().stream()
                .map(ContainerBaseResponse::getId) // or .map(container -> container.getId())
                .filter(Objects::nonNull)
                .toList();
        if (!CollectionUtils.isEmpty(containersId)) {
            List<Packing> packs;
            if (CommonUtils.canFetchDetailsWithoutTenantFilter(xSource))
                packs = packingDao.findByContainerIdInWithoutTenantFilter(containersId);
            else
                packs = packingDao.findByContainerIdIn(containersId);
            AtomicLong assignedContainerCount = new AtomicLong(0L);
            AtomicLong unassignedContainerCount = new AtomicLong(0L);
            Set<Long> uniqueContainers = packs.stream().map(Packing::getContainerId).collect(Collectors.toSet());
            containerListResponse.getContainers().forEach(containerBaseResponse -> {
                if (uniqueContainers.contains(containerBaseResponse.getId())) {
                    containerBaseResponse.setAssignedContainer(Constants.YES);
                    containerBaseResponse.setIsContainerAssigned(true);
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

    public ContainerSummaryResponse getContainerSummaryResponse(List<Containers> containersList, boolean isShipment, String xSource) throws RunnerException { //NOSONAR
        double totalWeight = 0;
        double tareWeight = 0;
        double totalVolume = 0;
        double totalContainerCount = 0;
        double totalPacks = 0;
        String packsType = null;
        int dgContainers = 0;
        double netWeight = 0;
        long assignedContainers = 0;
        double totalDgPackages = 0;
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
                totalVolume = totalVolume + volume;
                totalContainerCount = getTotalContainerCount(containers, totalContainerCount);
                totalPacks = getTotalPacks(containers, totalPacks);
                packsType = commonUtils.getPacksUnit(packsType, containers.getPacksType());
                totalDgPackages = getTotalDGPacks(containers, totalDgPackages);
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
                if (CommonUtils.canFetchDetailsWithoutTenantFilter(xSource))
                    shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerIdInWithoutTenantFilter(containersList.stream().map(BaseEntity::getId).toList());
                else
                    shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerIdIn(containersList.stream().map(BaseEntity::getId).toList());
                assignedContainers = shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getContainerId).distinct().count();
            }
            groupedContainerSummaryList = new ArrayList<>(summaryMap.values());
        }
        ContainerSummaryResponse response = new ContainerSummaryResponse();
        response.setTotalPackages(String.format(Constants.STRING_FORMAT, IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalPacks), 0, v1TenantSettingsResponse), commonUtils.getPacksUnit(packsType)));
        response.setTotalContainers(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalContainerCount), 0, v1TenantSettingsResponse));
        response.setTotalWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalTareWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(tareWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalNetWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(netWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalContainerVolume(String.format(Constants.STRING_FORMAT, IReport.convertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), v1TenantSettingsResponse), toVolumeUnit));
        if (response.getSummary() == null)
            response.setSummary("");
        setContainerSummary(containersList, response);
        response.setDgContainers(dgContainers);
        response.setTotalDgPackages((long) totalDgPackages);
        response.setTotalPackagesWithoutUnit((long) totalPacks);
        setAssignedContainerCount(containersList, isShipment, assignedContainers, v1TenantSettingsResponse, response);
        response.setGroupedContainersSummary(groupedContainerSummaryList);
        return response;
    }

    double getTotalDGPacks(Containers containers, double totalDgPackages) {
        if(containers.getPacksList() != null) {
            for (Packing packing : containers.getPacksList()) {
                if (Boolean.TRUE.equals(packing.getHazardous())) totalDgPackages += Long.valueOf(packing.getPacks());
            }
        }

        return totalDgPackages;
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
        // Convert to UPPERCASE
        containerNumber = containerNumber.toUpperCase();

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
        // Validate 4th character must be U, J, or Z (warning only)
        char fourthChar = containerNumber.charAt(3);
        if (fourthChar != 'U' && fourthChar != 'J' && fourthChar != 'Z') {
            response.setWarningMessage("Invalid Container Number. The fourth character must be U, J, or Z.");
            // Still allow proceeding with success
            response.setSuccess(true);
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
            Pair<Specification<Containers>, Pageable> tuple;
            if(ObjectUtils.isEmpty(request.getContainsText())){
                tuple = fetchData(request, Containers.class);
            } else {
                tuple = fetchData(request, Containers.class, ContainerConstants.TABLES_NAMES);
            }
            // construct specifications for filter request

            Page<Containers> containersPage;
            if (CommonUtils.canFetchDetailsWithoutTenantFilter(xSource))
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

    public Map<String, Object> getMasterDataForList(List<ContainerBaseResponse> responseList, boolean getMasterData) {
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

    private void handlePostSaveActions(Containers container, ContainerV3Request request, String module) throws RunnerException {
        if (Objects.equals(BOOKING, module)) {
            customerBookingV3Service.updateContainerInfoInBooking(request.getBookingId());
        }
        afterSave(container, true, true);
    }

  private void handlePostSaveActionsBulk(List<Containers> containers, List<ContainerV3Request> requests, String module) {
    if (!Set.of(SHIPMENT, CONSOLIDATION).contains(module)) return;

    for (int i = 0; i < containers.size(); i++) {
      Containers container = containers.get(i);
      ContainerV3Request request = requests.get(i); // assuming same index mapping

      afterSave(container, true, true);

      // Shipment assignment (sync)
      Optional.ofNullable(module)
          .filter(SHIPMENT::equals)
          .filter(m -> container.getId() != null)
          .filter(m -> request.getShipmentId() != null)
          .ifPresent(m -> shipmentsContainersMappingDao.assignShipments(
              container.getId(),
              Set.of(request.getShipmentId()),
              false
          ));
    }
  }

    private void runAsyncPostSaveOperations(List<Containers> containers, boolean isAutoSell, String module) throws RunnerException {
        if (Objects.equals(BOOKING, module)) {
            customerBookingV3Service.updateContainerInfoInBooking(containers.iterator().next().getBookingId());
        }
        for (Containers container : containers) {
            afterSave(container, isAutoSell, false);
        }
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
                afterSave(container, false, isCreate);
            }
        }
    }

    public Containers setAssignContainerParams(AssignContainerRequest request, String module, AssignContainerParams assignContainerParams) throws RunnerException {
        request.setShipmentPackIds(request.getShipmentPackIds().entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        e -> e.getValue() == null ? new ArrayList<>() : e.getValue()
                )));

        Containers container = fetchDataForAssignContainer(request, assignContainerParams);
        containerValidationUtil.validateBeforeAssignContainer(assignContainerParams, request, module);
        return container;
    }

    @PostConstruct
    private void setDefaultIncludeColumns() {
        defaultIncludeColumns = FieldUtils.getNonRelationshipFields(Containers.class);
        defaultIncludeColumns.addAll(List.of("id", "guid", "tenantId"));
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public ContainerResponse assignContainers(AssignContainerRequest request, String module) throws RunnerException {

        // Build OldShipmentDetails before unAssignment
        AssignContainerParams assignContainerParams = new AssignContainerParams();
        Containers container = setAssignContainerParams(request, module, assignContainerParams);

        // Build packingId list from request.shipmentPackIds
        List<Packing> packingList = fetchAndValidatePackings(request);

        // Adding Reassignment functionality
        if (Boolean.TRUE.equals(request.getAllowPackageReassignment())) {
            handleReassignment(request, module, packingList);
        } else {
            validateForNewAssignment(packingList);
        }

        ContainerResponse containerResponse = calculateAndSaveAssignContainerResults(container, assignContainerParams, request, module);

        pushShipmentDataToDependantService(request.getShipmentPackIds().keySet());

        return containerResponse;
    }

    private void pushShipmentDataToDependantService(Set<Long> shipmentIds) {
        if (shipmentIds == null || shipmentIds.isEmpty()) {
            log.info("No shipmentIds provided, skipping push to dependent service.");
            return;
        }

        try {
            List<ShipmentDetails> shipments = shipmentDao.findShipmentsByIds(shipmentIds);

            if (shipments == null || shipments.isEmpty()) {
                log.info("No shipments found for IDs: {}", shipmentIds);
                return;
            }

            for (ShipmentDetails shipment : shipments) {
                if (shipment == null) {
                    log.info("Encountered null shipment while processing IDs: {}", shipmentIds);
                    continue;
                }
                shipmentService.triggerPushToDownStream(shipment, shipment, false);

            }
        } catch (Exception ex) {
            log.error("Unexpected error while fetching shipments for Pushing to Internal queue for IDs {}: {}",
                    shipmentIds, ex.getMessage(), ex);
        }
    }

    private void validateForNewAssignment(List<Packing> packingList) {
        for (Packing packing : packingList) {
            if (Objects.nonNull(packing.getContainerId())) {
                throw new ValidationException("Package is already assigned to another container.");
            }
        }
    }

    private List<Packing> fetchAndValidatePackings(AssignContainerRequest request) throws RunnerException {
        List<Long> allPackIds = request.getShipmentPackIds().values().stream()
                .filter(Objects::nonNull)
                .flatMap(List::stream)
                .collect(Collectors.toList());

        if (allPackIds.isEmpty() && Boolean.TRUE.equals(request.getAllowPackageReassignment())) {
            throw new RunnerException("At least one package is required for reassignment.");
        }
        return packingDao.findByIdIn(allPackIds);
    }

    private void handleReassignment(AssignContainerRequest request, String module, List<Packing> packingList) throws RunnerException {
        List<List<Long>> shipmentIdsForDetachmentList = new ArrayList<>();
        Map<String, List<Containers>> unassignedContainersToSave = new HashMap<>();
        List<UnAssignContainerParams> unAssignContainerParamsList = new ArrayList<>();

        // Supporting for multiple OldContainers in case of ReAssign Request
        // containerIdPacksMap = Map<oldContainerId, List<Packing>>
        Map<Long, List<Packing>> containerIdPacksMap = buildOldContainerPackMap(request, packingList);

        // Looping over all old containers and trigger unassign
        UnAssignContainerParams unAssignContainerParams = new UnAssignContainerParams();
        for (Map.Entry<Long, List<Packing>> entry : containerIdPacksMap.entrySet()) {
            UnAssignContainerRequest unAssignContainerRequest = buildUnAssignRequest(entry.getKey(), entry.getValue());
            self.unAssignContainers(unAssignContainerRequest, module, unAssignContainerParams,
                    unassignedContainersToSave, shipmentIdsForDetachmentList, unAssignContainerParamsList, Boolean.TRUE, Boolean.FALSE);
        }

        // Bulk save for all unassign calls
        self.saveUnAssignContainerResultsBatch(shipmentIdsForDetachmentList, unassignedContainersToSave, unAssignContainerParamsList, Boolean.FALSE, Boolean.FALSE);
    }

    private Map<Long, List<Packing>> buildOldContainerPackMap(AssignContainerRequest request, List<Packing> packingList) {
        Map<Long, List<Packing>> containerIdPacksMap = new HashMap<>();

        for (Packing packing : packingList) {
            // Container Id is null for current packing, skip unassignment
            if (Objects.isNull(packing.getContainerId())) {
                continue;
            }

            // If a pack is already in the target container, skip processing.
            if (Objects.equals(packing.getContainerId(), request.getContainerId())) {
                // Remove current packId from AssignContainerRequest.shipmentPackIds
                removePackIdFromAssignRequest(request, packing.getShipmentId(), packing.getId());
            } else {
                containerIdPacksMap.computeIfAbsent(packing.getContainerId(), k -> new ArrayList<>());
                containerIdPacksMap.get(packing.getContainerId()).add(packing);
            }
        }
        return containerIdPacksMap;
    }

    private UnAssignContainerRequest buildUnAssignRequest(Long oldContainerId, List<Packing> packings) {
        UnAssignContainerRequest unAssignRequest = new UnAssignContainerRequest();
        unAssignRequest.setContainerId(oldContainerId);

        // Building shipmentPackIds → Map<shipmentId, List<packingIds>>
        Map<Long, List<Long>> shipmentPackIds = new HashMap<>();
        for (Packing packing : packings) {
            shipmentPackIds.computeIfAbsent(packing.getShipmentId(), k -> new ArrayList<>())
                    .add(packing.getId());
        }
        unAssignRequest.setShipmentPackIds(shipmentPackIds);
        return unAssignRequest;
    }

    private void removePackIdFromAssignRequest(AssignContainerRequest request, Long shipmentId, Long packId) {
        if (request.getShipmentPackIds() != null && request.getShipmentPackIds().containsKey(shipmentId)) {
            List<Long> packList = request.getShipmentPackIds().get(shipmentId);
            packList.remove(packId);
            if (packList.isEmpty()) {
                request.getShipmentPackIds().remove(shipmentId);
            }
        }
    }


    public ContainerResponse calculateAndSaveAssignContainerResults(Containers container,
                                                                    AssignContainerParams assignContainerParams,
                                                                    AssignContainerRequest request, String module) throws RunnerException{
        List<Long> shipmentIdsForAttachment = assignContainerCalculationsAndLogic(assignContainerParams, request, container, module);
        container = saveAssignContainerResults(container, shipmentIdsForAttachment, assignContainerParams);
        return jsonHelper.convertValue(container, ContainerResponse.class);
    }

    public void checkAndMakeDG(Containers container, List<Long> shipmentIdsForAttachment) {
        boolean isDG = false;
        boolean isDGClass1Added = false;
        boolean isDGContainer = false;
        boolean isDGPackage = false;
        if(Boolean.TRUE.equals(container.getHazardous())) {
            log.debug("Container {} is marked hazardous", container.getId());
            isDGClass1Added = commonUtils.checkIfDGClass1(container.getDgClass());
            isDG = true;
            isDGContainer = true;
        }

        List<Packing> containerPackings = packingDao.findByContainerIdIn(List.of(container.getId()));

        if(containerPackings != null) {
            for (Packing packing : containerPackings) {
                if (Boolean.TRUE.equals(packing.getHazardous())) {
                    log.debug("Packing {} is hazardous", packing.getId());
                    isDGClass1Added = isDGClass1Added || commonUtils.checkIfDGClass1(packing.getDGClass());
                    isDG = true;
                    isDGPackage = true;
                }
            }
        }

        log.info("DG packages is present {}, DG container is present {}, Container {} is not marked hazardous", isDGPackage, isDGContainer, container.getId());
        if(!isDGContainer && isDGPackage ){
            log.error("DG packages found but container {} is not marked hazardous", container.getId());
            throw new ValidationException(OCEAN_DG_CONTAINER_FIELDS_VALIDATION);
        }

        if(isDG) {
            for (Long shipmentId : shipmentIdsForAttachment) {
                saveDGShipment(shipmentId, isDGClass1Added);
            }
        }
    }

    private void saveDGShipment(Long shipmentId, boolean isDGClass1Added) {
        Optional<ShipmentDetails> optionalShipmentDetails = shipmentDao.findById(shipmentId);
        if(!optionalShipmentDetails.isPresent())
            return;
        ShipmentDetails shipmentDetails = optionalShipmentDetails.get();

        boolean saveShipment = !Boolean.TRUE.equals(shipmentDetails.getContainsHazardous());
        shipmentDetails.setContainsHazardous(true);
        saveShipment = saveShipment || commonUtils.changeShipmentDGStatusToReqd(shipmentDetails, isDGClass1Added);
        if(saveShipment) {
            String oceanDGStatus = shipmentDetails.getOceanDGStatus() != null ? shipmentDetails.getOceanDGStatus().name() : null;
            shipmentDao.updateDgStatusInShipment(shipmentDetails.getContainsHazardous(), oceanDGStatus, shipmentId);
        }
    }
    private Containers fetchDataForAssignContainer(AssignContainerRequest request, AssignContainerParams assignContainerParams) throws RunnerException {
        Long containerId = request.getContainerId();
        Set<Long> shipmentIdsRequestedList = request.getShipmentPackIds().keySet();
        List<Long> packIdsRequestedList = request.getShipmentPackIds().values()
                .stream()
                .filter(Objects::nonNull)
                .flatMap(List::stream)
                .filter(Objects::nonNull).toList();
        Set<Long> shipmentIds = new HashSet<>(shipmentIdsRequestedList);
        assignContainerParams.getAssignedPacks().addAll(packingDao.findByContainerIdIn(List.of(containerId)).stream().toList());
        assignContainerParams.getShipmentsContainersMappings().addAll(shipmentsContainersMappingDao.findByContainerId(containerId));
        assignContainerParams.getAssignedShipIds().addAll(assignContainerParams.getShipmentsContainersMappings().stream().map(ShipmentsContainersMapping::getShipmentId).toList());
        shipmentIds.addAll(assignContainerParams.getAssignedShipIds());
        Containers container = containerDao.findById(containerId)
                .orElseThrow(() -> new EntityNotFoundException("Container not found with ID: " + containerId));
        if (!listIsNullOrEmpty(packIdsRequestedList)) {
            assignContainerParams.getPackingListMap().putAll(packingDao.findByIdIn(packIdsRequestedList).stream().collect(Collectors.toMap(Packing::getId, Function.identity())));
        }
        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(shipmentIds);
        assignContainerParams.getShipmentDetailsMap().putAll(shipmentDetails.stream().collect(Collectors.toMap(ShipmentDetails::getId, Function.identity())));

        assignContainerParams.setConsolidationId(container.getConsolidationId());
        for(Long shipmentId: shipmentIdsRequestedList) {
            if(assignContainerParams.getShipmentDetailsMap().containsKey(shipmentId) &&
                    (Constants.CARGO_TYPE_FCL.equalsIgnoreCase(assignContainerParams.getShipmentDetailsMap().get(shipmentId).getShipmentType()) ||
                            Constants.CARGO_TYPE_FTL.equalsIgnoreCase(assignContainerParams.getShipmentDetailsMap().get(shipmentId).getShipmentType()))) {
                assignContainerParams.getFclOrFtlShipmentIds().add(shipmentId);
            }
        }
        setAssignedContainersParems(assignContainerParams);
        containerV3Util.resetContainerDataForRecalculation(container);
        return container;
    }

    public void setAssignedContainersParems(AssignContainerParams assignContainerParams) throws RunnerException {
        if(!setIsNullOrEmpty(assignContainerParams.getFclOrFtlShipmentIds())) {
            ConsolidationDetails consolidationDetails = consolidationV3Service.fetchConsolidationDetails(assignContainerParams.getConsolidationId());
            assignContainerParams.setConsolidationDetails(consolidationDetails);
            assignContainerParams.setOldShipmentWtVolResponse(consolidationV3Service.calculateShipmentWtVol(consolidationDetails));
        }
    }

    protected List<Long> assignContainerCalculationsAndLogic(AssignContainerParams assignContainerParams,
                                                           AssignContainerRequest request, Containers container, String module) throws RunnerException {
        List<Long> shipmentIdsForAttachment = new ArrayList<>();
        for (Long id : request.getShipmentPackIds().keySet()) {
            ShipmentDetails shipmentDetails = assignContainerParams.getShipmentDetailsMap().get(id);
            if (!assignContainerParams.getAssignedShipIds().contains(id)) {
                shipmentIdsForAttachment.add(id); // need to assign this shipment
            }
            if (listIsNullOrEmpty(request.getShipmentPackIds().get(id))) { // zero packages came for this shipment
                assignContainerOnlyToShipment(shipmentDetails, container, assignContainerParams.getShipmentIdsToSetContainerCargo());
            } else { // assigning some packages
                assignContainerToShipmentAndPackages(shipmentDetails, request, container, assignContainerParams.getPackingListMap(), assignContainerParams.getShipmentIdsToRemoveContainerCargo(), module);
            }
        }
        if (!listIsNullOrEmpty(assignContainerParams.getAssignedPacks())) { // adding weight/volume of already assigned packs
            for (Packing assignedPack : assignContainerParams.getAssignedPacks()) {
                addPackageDataToContainer(container, assignedPack);
            }
        }
        for(Long id: assignContainerParams.getAssignedShipIds()) { // adding weight/volume of already assigned Shipment Cargo
            ShipmentDetails shipmentDetails = assignContainerParams.getShipmentDetailsMap().get(id);
            if(Objects.equals(shipmentDetails.getContainerAssignedToShipmentCargo(), container.getId()))
                addShipmentCargoToContainer(container, shipmentDetails);
        }
        containerV3Util.setContainerNetWeight(Collections.singletonList(container)); // set container gross weight from cargo weight (net weight) and tare weight
        return shipmentIdsForAttachment;
    }
    public void assignContainerOnlyToShipment(ShipmentDetails shipmentDetails, Containers container,
                                               List<Long> shipmentIdsToSetContainerCargo) throws RunnerException {
        if(commonUtils.isSeaFCLOrRoadFTL(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType())) {
            throw new ValidationException("Please select atleast one package for FCL/FTL shipment.");
        }
        if(shipmentDetails.getContainerAssignedToShipmentCargo() != null) {
            throw new ValidationException(String.format(Constants.STRING_FORMAT, CONTAINER_ALREADY_ASSIGNED_MSG,
                    containerV3Util.getContainerNumberOrType(shipmentDetails.getContainerAssignedToShipmentCargo())));
        }
        Long assignedCont = checkIfAnyPackIsAssignedToContainer(shipmentDetails);
        if(!Objects.equals(assignedCont, 0L)) {
            throw new ValidationException(String.format(Constants.STRING_FORMAT, CONTAINER_ALREADY_ASSIGNED_MSG,
                    containerV3Util.getContainerNumberOrType(assignedCont)));
        }

        shipmentIdsToSetContainerCargo.add(shipmentDetails.getId()); // assign container to shipment cargo
        shipmentDetails.setContainerAssignedToShipmentCargo(container.getId());
        addShipmentCargoToContainer(container, shipmentDetails);
    }
    @Override
    public void addShipmentCargoToContainerInCreateFromBooking(Containers container, CustomerBookingV3Request customerBookingV3Request) throws RunnerException {
        containerV3Util.setWtVolUnits(container);
        container.setGrossWeight(containerV3Util.getAddedWeight(container.getGrossWeight(), container.getGrossWeightUnit(), customerBookingV3Request.getGrossWeight(), customerBookingV3Request.getGrossWeightUnit()));
        container.setGrossVolume(containerV3Util.getAddedVolume(container.getGrossVolume(), container.getGrossVolumeUnit(), customerBookingV3Request.getVolume(), customerBookingV3Request.getVolumeUnit()));
        containerV3Util.addNoOfPackagesValueToContainer(container, String.valueOf(customerBookingV3Request.getPackages()), customerBookingV3Request.getPackageType());
    }
    @Override
    public void addShipmentCargoToContainer(Containers container, ShipmentDetails shipmentDetails) throws RunnerException {
        containerV3Util.setWtVolUnits(container, shipmentDetails);
        container.setGrossWeight(containerV3Util.getAddedWeight(container.getGrossWeight(), container.getGrossWeightUnit(), shipmentDetails.getWeight(), shipmentDetails.getWeightUnit()));
        container.setGrossVolume(containerV3Util.getAddedVolume(container.getGrossVolume(), container.getGrossVolumeUnit(), shipmentDetails.getVolume(), shipmentDetails.getVolumeUnit()));
        containerV3Util.addNoOfPackagesToContainer(container, shipmentDetails.getNoOfPacks(), shipmentDetails.getPacksUnit());
    }
    private void assignContainerToShipmentAndPackages(ShipmentDetails shipmentDetails, AssignContainerRequest request, Containers container,
                                                      Map<Long, Packing> packingListMap,
                                                      List<Long> shipmentIdsToRemoveContainerCargo, String module) throws RunnerException {
        handleValidationOrDetachmentIfCargoSummaryAlreadyAttached(shipmentDetails, request, container, shipmentIdsToRemoveContainerCargo, module);
        for (Long packingId : request.getShipmentPackIds().get(shipmentDetails.getId())) { // assigning new packs and adding its weight/volume
            Packing packing = packingListMap.get(packingId);
            packing.setContainerId(container.getId());
            addPackageDataToContainer(container, packing);
        }
    }
    private void handleValidationOrDetachmentIfCargoSummaryAlreadyAttached(ShipmentDetails shipmentDetails, AssignContainerRequest request,
                                                                           Containers container, List<Long> shipmentIdsToRemoveContainerCargo,
                                                                           String module) throws RunnerException {
        if (shipmentDetails.getContainerAssignedToShipmentCargo() != null && !containerValidationUtil.checkIfShipmentIsFclOrFtl(shipmentDetails)) {
            if(!Boolean.TRUE.equals(request.getAllowCargoDetachIfRequired())) {
                containerValidationUtil.validateCanAssignPackageToContainer(shipmentDetails, module);
            } else {
                if(shipmentDetails.getContainerAssignedToShipmentCargo().equals(container.getId())) {
                    shipmentIdsToRemoveContainerCargo.add(shipmentDetails.getId());
                    shipmentDetails.setContainerAssignedToShipmentCargo(null);
                }
                else {
                    UnAssignContainerRequest unAssignContainerRequest = new UnAssignContainerRequest();
                    unAssignContainerRequest.setContainerId(shipmentDetails.getContainerAssignedToShipmentCargo());
                    unAssignContainerRequest.setShipmentPackIds(Map.of(shipmentDetails.getId(), new ArrayList<>()));
                    self.unAssignContainers(unAssignContainerRequest, CONTAINER_INTERNAL_CALL, new UnAssignContainerParams(), null, null, null, Boolean.FALSE, Boolean.FALSE);
                }
            }
        }
    }
    public void addPackageDataToContainer(Containers container, Packing packing) throws RunnerException {
        containerV3Util.setWtVolUnits(container, packing);
        container.setGrossWeight(containerV3Util.getAddedWeight(container.getGrossWeight(), container.getGrossWeightUnit(), packing.getWeight(), packing.getWeightUnit()));
        container.setGrossVolume(containerV3Util.getAddedVolume(container.getGrossVolume(), container.getGrossVolumeUnit(), packing.getVolume(), packing.getVolumeUnit()));
        containerV3Util.addNoOfPackagesValueToContainer(container, packing.getPacks(), packing.getPacksType());
    }
    private Containers saveAssignContainerResults(Containers container, List<Long> shipmentIdsForAttachment,
                                                  AssignContainerParams assignContainerParams) throws RunnerException {
        if (!listIsNullOrEmpty(assignContainerParams.getShipmentIdsToSetContainerCargo()))
            shipmentDao.setShipmentIdsToContainer(assignContainerParams.getShipmentIdsToSetContainerCargo(), container.getId());
        if(!listIsNullOrEmpty(assignContainerParams.getShipmentIdsToRemoveContainerCargo()))
            shipmentDao.setShipmentIdsToContainer(assignContainerParams.getShipmentIdsToRemoveContainerCargo(), null);
        if (!assignContainerParams.getPackingListMap().isEmpty() && !listIsNullOrEmpty(assignContainerParams.getPackingListMap().values().stream().toList()))
            packingDao.saveAll(assignContainerParams.getPackingListMap().values().stream().toList());
        container = containerDao.save(container);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>();
        for (Long id : shipmentIdsForAttachment) {
            ShipmentsContainersMapping entity = new ShipmentsContainersMapping();
            entity.setShipmentId(id);
            entity.setContainerId(container.getId());
            shipmentsContainersMappingList.add(entity);
        }
        if (!listIsNullOrEmpty(shipmentsContainersMappingList))
            shipmentsContainersMappingDao.saveAll(shipmentsContainersMappingList);
        checkAndMakeDG(container, shipmentIdsForAttachment);
        updateSummary(container, shipmentIdsForAttachment, assignContainerParams);
        return container;
    }
    public void updateSummary(Containers container, List<Long> shipmentIdsForAttachment, AssignContainerParams assignContainerParams) throws RunnerException {
        if(!setIsNullOrEmpty(assignContainerParams.getFclOrFtlShipmentIds())) {
            for(Long shipmentId: assignContainerParams.getFclOrFtlShipmentIds()) {
                ShipmentDetails shipmentDetails = assignContainerParams.getShipmentDetailsMap().get(shipmentId);
                List<Containers> containersList = new ArrayList<>(shipmentDetails.getContainersList());
                if(shipmentIdsForAttachment.contains(shipmentDetails.getId()))
                    containersList.add(container);
                shipmentService.calculateAndUpdateShipmentCargoSummary(shipmentDetails, containersList);
                if(Objects.equals(shipmentDetails.getMigrationStatus(), MigrationStatus.MIGRATED_FROM_V2)) {
                    shipmentDetails.setTriggerMigrationWarning(false);
                    shipmentDao.updateTriggerMigrationWarning(shipmentId);
                }
            }
            consolidationV3Service.updateConsolidationCargoSummary(assignContainerParams.getConsolidationDetails(), assignContainerParams.getOldShipmentWtVolResponse());
        }
    }
    private Long checkIfAnyPackIsAssignedToContainer(ShipmentDetails shipmentDetails) {
        for (Packing packing : shipmentDetails.getPackingList()) {
            if (packing.getContainerId() != null)
                return packing.getContainerId();
        }
        return 0L;
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
    @Transactional(rollbackFor = Exception.class)
    public ContainerResponse unAssignContainers(UnAssignContainerRequest request,
                                                String module,
                                                UnAssignContainerParams unAssignContainerParams,
                                                Map<String, List<Containers>> unassignedContainersToSave,
                                                List<List<Long>> shipmentIdsForDetachmentList,
                                                List<UnAssignContainerParams> unAssignContainerParamsList,
                                                Boolean allowPackageReassignment, Boolean isForcedDetach) throws RunnerException {
        // make sure pack ids is empty (never null)
        request.setShipmentPackIds(request.getShipmentPackIds().entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        e -> e.getValue() == null ? new ArrayList<>() : e.getValue()
                )));
        Containers container = fetchDataForUnAssignContainer(request, unAssignContainerParams);
        containerValidationUtil.validateBeforeUnAssignContainer(unAssignContainerParams, request, module, isForcedDetach);
        List<Long> shipmentIdsForDetachment = unAssignContainerCalculationsAndLogic(request, container, unAssignContainerParams,
                                                                                    unassignedContainersToSave, isForcedDetach);
        if (Boolean.TRUE.equals(allowPackageReassignment) || Boolean.TRUE.equals(isForcedDetach)) {
            unassignedContainersToSave.computeIfAbsent("containersToSave", k -> new ArrayList<>()).add(container);
            shipmentIdsForDetachmentList.add(shipmentIdsForDetachment);
            unAssignContainerParamsList.add(unAssignContainerParams);
            pushShipmentDataToDependantService(new HashSet<>(shipmentIdsForDetachment));
            return null;
        } else {
            container = saveUnAssignContainerResults(shipmentIdsForDetachment, container, unAssignContainerParams);
            pushShipmentDataToDependantService(new HashSet<>(shipmentIdsForDetachment));
            return jsonHelper.convertValue(container, ContainerResponse.class);
        }
    }

    private Containers fetchDataForUnAssignContainer(UnAssignContainerRequest request, UnAssignContainerParams unAssignContainerParams) throws RunnerException {
        Long containerId = request.getContainerId();
        Containers container = containerDao.findById(containerId)
                .orElseThrow(() -> new EntityNotFoundException("Container not found with ID: " + containerId));
        unAssignContainerParams.getShipmentsContainersMappings().addAll(shipmentsContainersMappingDao.findByContainerId(containerId));
        Set<Long> allAssignedShipmentIds = unAssignContainerParams.getShipmentsContainersMappings().stream()
                .map(ShipmentsContainersMapping::getShipmentId)
                .collect(Collectors.toSet());
        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findShipmentsByIds(allAssignedShipmentIds);
        unAssignContainerParams.getShipmentDetailsMap().putAll(shipmentDetailsList.stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity())));
        allAssignedShipmentIds.forEach(id -> unAssignContainerParams.getShipmentPackingMap().put(id, new ArrayList<>()));
        List<Packing> packings = packingDao.findByShipmentIdInAndContainerId(allAssignedShipmentIds.stream().toList(), containerId);
        for (Packing packing : packings) {
            Long shipmentId = packing.getShipmentId();
            unAssignContainerParams.getShipmentPackingMap()
                    .computeIfAbsent(shipmentId, k -> new ArrayList<>())
                    .add(packing);
        }
        unAssignContainerParams.setConsolidationId(container.getConsolidationId());
        // check fcl/ftl shipment and fetch old shipment wt vol for recalculation
        for(Long shipmentId: request.getShipmentPackIds().keySet()) {
            if(unAssignContainerParams.getShipmentDetailsMap().containsKey(shipmentId)) {
                ShipmentDetails shipmentDetails = unAssignContainerParams.getShipmentDetailsMap().get(shipmentId);
                if(commonUtils.isSeaFCLOrRoadFTL(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType())) {
                    unAssignContainerParams.getFclOrFtlShipmentIds().add(shipmentId);
                }
            }
        }
        setUnassignedContainerParems(unAssignContainerParams);
        unAssignContainerParams.setOldContainersEntity(jsonHelper.convertValue(container, Containers.class));
        containerV3Util.resetContainerDataForRecalculation(container);
        return container;
    }

    public void setUnassignedContainerParems(UnAssignContainerParams unAssignContainerParams) throws RunnerException {
        if(!setIsNullOrEmpty(unAssignContainerParams.getFclOrFtlShipmentIds()) && Objects.isNull(unAssignContainerParams.getConsolidationDetails())) {
            ConsolidationDetails consolidationDetails = consolidationV3Service.fetchConsolidationDetails(unAssignContainerParams.getConsolidationId());
            unAssignContainerParams.setConsolidationDetails(consolidationDetails);
            unAssignContainerParams.setOldShipmentWtVolResponse(consolidationV3Service.calculateShipmentWtVol(consolidationDetails));
        }
    }

    private List<Long> unAssignContainerCalculationsAndLogic(UnAssignContainerRequest request, Containers container,
                                                             UnAssignContainerParams unAssignContainerParams,
                                                             Map<String, List<Containers>> unassignedContainersToSave,
                                                             Boolean isForcedDetach) throws RunnerException {
        List<Long> shipmentIdsForDetachment = new ArrayList<>();

        for (Map.Entry<Long, ShipmentDetails> entry : unAssignContainerParams.getShipmentDetailsMap().entrySet()) {
            Long shipmentId = entry.getKey();
            ShipmentDetails shipmentDetails = entry.getValue();
            List<Packing> packingList = unAssignContainerParams.getShipmentPackingMap().get(shipmentId);
            if (request.getShipmentPackIds().containsKey(shipmentId)) { // Shipment came for some/all packs detachment
                detachPacksAndShipmentFromContainer(request, container, packingList, shipmentIdsForDetachment, shipmentDetails,
                                                    unAssignContainerParams, unassignedContainersToSave, isForcedDetach);
            } else { // Shipment and its packages remains intact i.e. not being detached
                addExistingShipmentAndPackagesToContainer(shipmentDetails, container, packingList);
            }
        }
        containerV3Util.setContainerNetWeight(Collections.singletonList(container)); // set container net weight from gross weight and tare weight
        return shipmentIdsForDetachment;
    }

    private void detachPacksAndShipmentFromContainer(UnAssignContainerRequest request, Containers container, List<Packing> packingList,
                                                     List<Long> shipmentIdsForDetachment, ShipmentDetails shipmentDetails,
                                                     UnAssignContainerParams unAssignContainerParams,
                                                     Map<String, List<Containers>> unassignedContainersToSave, Boolean isForcedDetach) throws RunnerException {
        Set<Long> removePackIds = new HashSet<>(request.getShipmentPackIds().get(shipmentDetails.getId()));
        // we are removing all the packages from this shipment, hence container will be detached from shipment (but not for FCL/FTL shipment)
        if (Objects.equals(removePackIds.size(), packingList.size())) {
            handleUnAssignmentLogicWhenAllPacksAreRemoved(unAssignContainerParams, container, shipmentDetails, shipmentIdsForDetachment,
                                                            packingList, removePackIds, unassignedContainersToSave, isForcedDetach);
        } else { // only some packages are being removed from container
            handleUnAssignmentLogicWhenOnlyFewPacksAreRemoved(unAssignContainerParams, container, packingList, removePackIds);
        }
    }

    private void handleUnAssignmentLogicWhenAllPacksAreRemoved(UnAssignContainerParams unAssignContainerParams, Containers container,
                                                               ShipmentDetails shipmentDetails, List<Long> shipmentIdsForDetachment,
                                                               List<Packing> packingList, Set<Long> removePackIds,
                                                               Map<String, List<Containers>> unassignedContainersToSave,Boolean isForcedDetach) {
        Long shipmentId = shipmentDetails.getId();
        //add isForceDetach flag
        if(commonUtils.isSeaFCLOrRoadFTL(shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType()) ) {
            if(Boolean.TRUE.equals(isForcedDetach)){
                shipmentIdsForDetachment.add(shipmentId);
                unassignedContainersToSave.computeIfAbsent("FCLContainersToRemove", k -> new ArrayList<>()).add(container);
            }else {
                container.setGrossWeight(unAssignContainerParams.getOldContainersEntity().getGrossWeight());
                container.setGrossWeightUnit(unAssignContainerParams.getOldContainersEntity().getGrossWeightUnit());
                container.setGrossVolume(unAssignContainerParams.getOldContainersEntity().getGrossVolume());
                container.setGrossVolumeUnit(unAssignContainerParams.getOldContainersEntity().getGrossVolumeUnit());
                container.setPacks(unAssignContainerParams.getOldContainersEntity().getPacks());
                container.setPacksType(unAssignContainerParams.getOldContainersEntity().getPacksType());
                // to remove FCL container in case of force detach
            }

        }
        else {
            shipmentIdsForDetachment.add(shipmentId);
        }
        unAssignContainerParams.getRemoveAllPackingIds().addAll(removePackIds);

        if (Objects.equals(shipmentDetails.getContainerAssignedToShipmentCargo(),
                container.getId())) { // shipment cargo was linked to this container
            unAssignContainerParams.getShipmentIdsForCargoDetachment().add(shipmentId);
            shipmentDetails.setContainerAssignedToShipmentCargo(null); // check if required
        }
        packingList.forEach(e -> e.setContainerId(null));
    }

    private void handleUnAssignmentLogicWhenOnlyFewPacksAreRemoved(UnAssignContainerParams unAssignContainerParams, Containers container,
                                                                   List<Packing> packingList, Set<Long> removePackIds) throws RunnerException {
        unAssignContainerParams.getRemoveAllPackingIds().addAll(removePackIds);
        // add data of remaining packages to container
        for (Packing packing : packingList) { // loop over all the assigned packs of shipment
            if (!removePackIds.contains(packing.getId())) { // this pack is not being detached
                addPackageDataToContainer(container, packing);
            } else { // this pack is being detached
                packing.setContainerId(null);
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

    private Containers saveUnAssignContainerResults(List<Long> shipmentIdsForDetachment, Containers container,
                                                    UnAssignContainerParams unAssignContainerParams) {
        if (!listIsNullOrEmpty(unAssignContainerParams.getShipmentIdsForCargoDetachment()))
            shipmentDao.setShipmentIdsToContainer(unAssignContainerParams.getShipmentIdsForCargoDetachment(), null);
        if (!listIsNullOrEmpty(unAssignContainerParams.getRemoveAllPackingIds()))
            packingDao.setPackingIdsToContainer(unAssignContainerParams.getRemoveAllPackingIds(), null);
        container = containerDao.save(container);
        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>();
        for (ShipmentsContainersMapping shipmentsContainersMapping : unAssignContainerParams.getShipmentsContainersMappings()) {
            if (shipmentIdsForDetachment.contains(shipmentsContainersMapping.getShipmentId())) {
                shipmentsContainersMappingList.add(shipmentsContainersMapping);
            }
        }
        if (!listIsNullOrEmpty(shipmentsContainersMappingList))
            shipmentsContainersMappingDao.deleteAll(shipmentsContainersMappingList);
        return container;
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void saveUnAssignContainerResultsBatch(List<List<Long>> allShipmentIdsForDetachment,
                                                  Map<String, List<Containers>> containersToSaveMap,
                                                  List<UnAssignContainerParams> globalUnAssignContainerParams, Boolean isFCLDelete, boolean fromDelete) {

        List<ShipmentsContainersMapping> shipmentsContainersMappingList = new ArrayList<>();

        for (int currUnAssignContainerParams = 0; currUnAssignContainerParams < globalUnAssignContainerParams.size(); currUnAssignContainerParams++) {

            // Batch update shipment IDs to container - same as ContainerV3Service
            if (!listIsNullOrEmpty(globalUnAssignContainerParams
                    .get(currUnAssignContainerParams)
                    .getShipmentIdsForCargoDetachment()))
                shipmentDao.setShipmentIdsToContainer(globalUnAssignContainerParams
                        .get(currUnAssignContainerParams)
                        .getShipmentIdsForCargoDetachment(), null);

            // Batch update packing IDs to container - same as ContainerV3Service
            if (!listIsNullOrEmpty(globalUnAssignContainerParams
                    .get(currUnAssignContainerParams)
                    .getRemoveAllPackingIds()))
                packingDao.setPackingIdsToContainer(globalUnAssignContainerParams
                        .get(currUnAssignContainerParams)
                        .getRemoveAllPackingIds(), null);


            // Collect shipments containers mappings for deletion
            collectShipmentContainerMapping(allShipmentIdsForDetachment, globalUnAssignContainerParams, currUnAssignContainerParams, shipmentsContainersMappingList);
        }

        // skipped from delete as associated shipments and containers are removed later already in function "deleteContainerAndAssociations"
        if(!fromDelete) {
            List<Containers> containersListToSave = containersToSaveMap.get("containersToSave");
            List<Containers> fclContainersListToRemove =  containersToSaveMap.get("FCLContainersToRemove");

            // early removing the fcl container from ship-container to avoid JPA issue in deleting containers
            removeFclContainersFromShipContainerMapping(isFCLDelete, containersListToSave, fclContainersListToRemove, shipmentsContainersMappingList);

            // Batch save all containers
            saveContainers(containersListToSave);

            // Batch delete shipments containers mappings
            deleteMappings(shipmentsContainersMappingList);

            if(Boolean.TRUE.equals(isFCLDelete) && fclContainersListToRemove != null && !fclContainersListToRemove.isEmpty()) {
                List<Long> containersIdToDelete = fclContainersListToRemove.stream().map(Containers::getId).collect(Collectors.toCollection(ArrayList::new));
                containerDao.deleteByIdIn(containersIdToDelete);
            }
        }

    }

    private static void collectShipmentContainerMapping(List<List<Long>> allShipmentIdsForDetachment, List<UnAssignContainerParams> globalUnAssignContainerParams, int currUnAssignContainerParams, List<ShipmentsContainersMapping> shipmentsContainersMappingList) {
        if (!listIsNullOrEmpty(globalUnAssignContainerParams.get(currUnAssignContainerParams).getShipmentsContainersMappings())) {
            for (ShipmentsContainersMapping shipmentsContainersMapping :
                    globalUnAssignContainerParams.get(currUnAssignContainerParams).getShipmentsContainersMappings()) {
                if (allShipmentIdsForDetachment.get(currUnAssignContainerParams).contains(shipmentsContainersMapping.getShipmentId())) {
                    shipmentsContainersMappingList.add(shipmentsContainersMapping);
                }
            }
        }
    }

    private static void removeFclContainersFromShipContainerMapping(Boolean isFCLDelete, List<Containers> containersListToSave, List<Containers> fclContainersListToRemove, List<ShipmentsContainersMapping> shipmentsContainersMappingList) {
        if(Boolean.TRUE.equals(isFCLDelete) && containersListToSave != null && fclContainersListToRemove != null) {
                List<Long> containersIdToDelete = fclContainersListToRemove.stream().map(Containers::getId).collect(Collectors.toCollection(ArrayList::new));

                for(Containers con: containersListToSave){
                    if( containersIdToDelete.contains(con.getId()) ){
                        shipmentsContainersMappingList.removeIf(smp -> smp.getContainerId().equals(con.getId()));
                        con.setShipmentsList(new HashSet<>());
                    }
                }
            }
    }

    private void saveContainers(List<Containers> containersToSave) {
        if (!listIsNullOrEmpty(containersToSave)) {
            containerDao.saveAll(containersToSave);
        }
    }

    private void deleteMappings(List<ShipmentsContainersMapping> mappings) {
        if (!listIsNullOrEmpty(mappings)) {
            shipmentsContainersMappingDao.deleteAll(mappings);
        }
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
        if (CommonUtils.listIsNullOrEmpty(containers))
            return;
        Map<Long, Containers> containersMap = containers.stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity()));
        for(Containers containers1: containers) {
            containerV3Util.resetContainerDataForRecalculation(containers1);
        }
        for(ShipmentDetails shipmentDetails: shipmentDetailsList.getContent()) {
            addShipmentCargoToContainer(containersMap.get(shipmentDetails.getContainerAssignedToShipmentCargo()), shipmentDetails);
        }
        if (!CommonUtils.listIsNullOrEmpty(packingList)) {
            for (Packing packing : packingList) {
                if (packing.getContainerId() != null && containersMap.containsKey(packing.getContainerId())) {
                    addPackageDataToContainer(containersMap.get(packing.getContainerId()), packing);
                }
            }
        }
        for(Containers containers1: containers) {
            containerV3Util.setContainerNetWeight(Collections.singletonList(containers1)); // set container net weight from gross weight and tare weight
        }
        containerDao.saveAll(containers.stream().toList());
    }

    public void pushContainersToDependentServices(List<Containers> containersList) {
        int size = containersList != null ? containersList.size() : 0;
        log.info("Starting pushContainersToDependentServices with containersList size: {}", size);

        if (CommonUtils.listIsNullOrEmpty(containersList)) {
            String errMsg = "Container list is null or empty. Exiting.";
            log.info(errMsg);
            throw new ValidationException(errMsg);
        }
        V1TenantSettingsResponse tenantSettings = commonUtils.getCurrentTenantSettings();
        log.debug("Tenant settings retrieved: LogicAppIntegrationEnabled={}, TransportOrchestratorEnabled={}",
                tenantSettings.getLogicAppIntegrationEnabled(),
                tenantSettings.getTransportOrchestratorEnabled());
        if (!canProcessContainers(containersList, tenantSettings)) {
            log.warn("Containers cannot be processed based on tenant settings. Exiting.");
            return;
        }
        List<ContainerPayloadDetails> payloadDetails = getContainerPayloadDetailsForExistingContainers(containersList);
        if (CommonUtils.listIsNullOrEmpty(payloadDetails)) {
            log.warn("No payload details found for containers. Exiting.");
            return;
        }
        EventMessage eventMessage = new EventMessage();
        eventMessage.setMessageType(ContainerConstants.CONTAINER_UPDATE_MSG);
        ContainerUpdateRequest updateRequest = new ContainerUpdateRequest();
        updateRequest.setContainers(payloadDetails);
        updateRequest.setTenantCode(UserContext.getUser().getCode());
        eventMessage.setContainerUpdateRequest(updateRequest);
        String jsonBody = jsonHelper.convertToJson(eventMessage);
        log.info("JSON body created for event message: {}", jsonBody);
        if (Boolean.TRUE.equals(tenantSettings.getTransportOrchestratorEnabled())) {
            log.info("Producing message to Kafka for transport orchestrator.");
            producer.produceToKafka(jsonBody, transportOrchestratorQueue, UUID.randomUUID().toString());
        }
        sbUtils.sendMessagesToTopic(isbProperties, messageTopic, List.of(new ServiceBusMessage(jsonBody)));
        log.info("Container pushed to Kafka and dependent services with data: {}", jsonBody);
    }
    private boolean canProcessContainers(List<Containers> containersList, V1TenantSettingsResponse tenantSettings) {
        boolean hasContainers = containersList != null && !containersList.isEmpty();
        boolean integrationEnabled = Boolean.TRUE.equals(tenantSettings.getLogicAppIntegrationEnabled());
        boolean orchestratorEnabled = Boolean.TRUE.equals(tenantSettings.getTransportOrchestratorEnabled());
        return hasContainers && (integrationEnabled || orchestratorEnabled);
    }
    private List<ContainerPayloadDetails> getContainerPayloadDetailsForExistingContainers(List<Containers> containersList) {
        List<ContainerPayloadDetails> payloadDetails = new ArrayList<>();

        for (Containers container : containersList) {
            Set<ShipmentDetails> shipments = container.getShipmentsList();
            boolean isValidContainer = StringUtility.isNotEmpty(container.getContainerNumber())
                    && ObjectUtils.isNotEmpty(shipments);
            if (!isValidContainer) {
                continue;
            }
            for (ShipmentDetails shipment : shipments) {
                String bookingRef = shipment.getBookingReference();
                if (StringUtility.isNotEmpty(bookingRef)) {
                    log.info("Platform Booking reference obtained: {}", bookingRef);
                    log.info("Preparing platform payload for container ID: {} with container number: {}",
                            container.getId(), container.getContainerNumber());

                    ContainerPayloadDetails detail = prepareQueuePayload(container, bookingRef);
                    payloadDetails.add(detail);
                }
            }
        }
        return payloadDetails;
    }
    private ContainerPayloadDetails prepareQueuePayload(Containers container, String bookingRef) {
        ContainerBoomiUniversalJson jsonPayload = modelMapper.map(container, ContainerBoomiUniversalJson.class);
        if (Boolean.TRUE.equals(jsonPayload.getHazardous())) {
            jsonPayload.setCargoType(ContainerConstants.HAZ);
            jsonPayload.setHazardousGoodType(container.getDgClass());
        }
        jsonPayload.setAllocationDate(
                commonUtils.getUserZoneTime(jsonPayload.getAllocationDate())
        );
        ContainerPayloadDetails payloadDetail = new ContainerPayloadDetails();
        payloadDetail.setBookingRef(bookingRef);
        payloadDetail.setContainer(jsonPayload);
        return payloadDetail;
    }
    private ListCommonRequest getEnrichedRequest(ListCommonRequest request) {
        ListCommonRequest enrichedRequest;

        if (CollectionUtils.isEmpty(request.getFilterCriteria())) {
            enrichedRequest = CommonUtils.constructListCommonRequest(
                    CONSOLIDATION_ID, Long.valueOf(request.getEntityId()), Constants.EQ);
        } else {
            enrichedRequest = CommonUtils.andCriteria(
                    CONSOLIDATION_ID, Long.valueOf(request.getEntityId()), Constants.EQ, request);
        }
        enrichedRequest.setSortRequest(request.getSortRequest());
        enrichedRequest.setPageNo(request.getPageNo());
        enrichedRequest.setPageSize(request.getPageSize());
        enrichedRequest.setContainsText(request.getContainsText());
        return enrichedRequest;
    }
    @Override
    public ContainerListResponse fetchConsolidationContainersForPackageAssignment(ListCommonRequest request, String module) throws RunnerException {
        ListCommonRequest enrichedRequest = getEnrichedRequest(request);
        ContainerListResponse containerListResponse;
        try {
            Pair<Specification<Containers>, Pageable> tuple;
            if(ObjectUtils.isEmpty(enrichedRequest.getContainsText())){
                tuple = fetchData(enrichedRequest, Containers.class);
            } else {
                tuple = fetchData(enrichedRequest, Containers.class, ContainerConstants.TABLES_NAMES);
            }
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Containers list for get containers retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            List<String> includeColumns = List.of("id", "guid", "tenantId", "containerNumber", "containerCode", "grossWeight", "grossWeightUnit", "grossVolume", "grossVolumeUnit");
            List<ContainerBaseResponse> responseList = convertEntityListWithFieldFilter(containersPage.getContent(), includeColumns);
            containerListResponse = ContainerListResponse.builder()
                    .containers(responseList)
                    .numberOfRecords(containersPage.getTotalElements())
                    .totalPages(containersPage.getTotalPages())
                    .build();
        } catch (Exception ex) {
            throw new IllegalArgumentException("Failed to fetch consolidation containers", ex);
        }
        return processAfterList(containerListResponse);
    }
    private ContainerListResponse processAfterList(ContainerListResponse containerListResponse) {
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
            setAttachedShipmentResponseInContainer(container, details);
        });
        return containerListResponse;
    }
    public static void setAttachedShipmentResponseInContainer(ContainerBaseResponse container, List<ShipmentDetailsProjection> details) {
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
    }
    public void updateContainerRequestOnDgFlag(List<ContainerV3Request> containerV3Requests) {
        for(ContainerV3Request containerRequest: containerV3Requests) {
            if(Boolean.FALSE.equals(containerRequest.getHazardous())) {
                updateContainerRequestWithDgFalse(containerRequest);
            }
        }
    }
    public void updateContainerRequestWithDgFalse(ContainerV3Request containerRequest) {
        containerRequest.setUnNumber(null);
        containerRequest.setProperShippingName(null);
        containerRequest.setDgClass(null);
        containerRequest.setMarinePollutant(null);
        containerRequest.setPackingGroup(null);
        containerRequest.setMinimumFlashPoint(null);
    }
}