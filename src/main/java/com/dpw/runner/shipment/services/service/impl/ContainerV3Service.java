package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerV3Service;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ContainerValidationUtil;
import com.dpw.runner.shipment.services.utils.ExcelCell;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

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

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    private final List<String> columnsSequenceForExcelDownload = List.of(
            "guid", "isOwnContainer", "isShipperOwned", "ownType", "isEmpty", "isReefer", "containerCode",
            "hblDeliveryMode", "containerNumber", "containerCount", "descriptionOfGoods", "handlingInfo",
            "hazardous", "dgClass", "hazardousUn", "packs", "packsType", "marksNums", "minTemp", "minTempUnit",
            "netWeight", "netWeightUnit", "grossWeight", "grossWeightUnit", "tareWeight", "tareWeightUnit", "grossVolume",
            "grossVolumeUnit", "measurement", "measurementUnit", "commodityCode", "hsCode", "customsReleaseCode",
            "containerStuffingLocation", "pacrNumber", "containerComments", "sealNumber", "carrierSealNumber",
            "shipperSealNumber", "terminalOperatorSealNumber", "veterinarySealNumber", "customsSealNumber"
    );

    private final List<String> columnsSequenceForExcelDownloadForAir = List.of(
            "guid", "hblDeliveryMode", "descriptionOfGoods", "hazardous", "hazardousUn", "packs", "packsType",
            "marksNums", "serialNumber", "innerPackageNumber", "innerPackageType", "packageLength", "packageBreadth",
            "packageHeight", "innerPackageMeasurementUnit", "isTemperatureMaintained", "minTemp", "minTempUnit",
            "netWeight", "netWeightUnit", "grossWeight", "grossWeightUnit", "tareWeight", "tareWeightUnit", "chargeable",
            "chargeableUnit", "grossVolume", "grossVolumeUnit", "commodityCode", "hsCode", "customsReleaseCode", "pacrNumber",
            "containerComments"
    );

    @Override
    public ContainerResponse create(ContainerV3Request containerRequest) {
        String requestId = LoggerHelper.getRequestIdFromMDC();

        if (containerRequest == null) {
            log.error("Container create request is null | Request ID: {}", requestId);
            return null;
        }

        log.info("Starting container creation | Request ID: {} | Request Body: {}", requestId, containerRequest);

        // Convert DTO to Entity
        Containers container = jsonHelper.convertValue(containerRequest, Containers.class);
        log.debug("Converted container request to entity | Entity: {}", container);

        // Save to DB
        Containers savedContainer = containerRepository.save(container);
        log.info("Saved container entity to DB | Container ID: {} | Request ID: {}", savedContainer.getId(), requestId);

        // Post-save logic
        afterSave(savedContainer, true);
        log.debug("afterSave logic executed for container ID: {}", savedContainer.getId());

        // Audit logging
        recordAuditLogs(null, List.of(savedContainer), DBOperationType.CREATE);
        log.info("Audit log recorded for container creation | Container ID: {}", savedContainer.getId());

        ContainerResponse response = jsonHelper.convertValue(savedContainer, ContainerResponse.class);
        log.info("Returning container response | Container ID: {} | Response: {}", savedContainer.getId(), response);

        return response;
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

    @Override
    public ContainerSummaryResponse calculateContainerSummary(Long shipmentId, Long consolidationId) throws RunnerException {
        if(shipmentId == null && consolidationId == null) {
            throw new RunnerException("Please provide shipmentId and consolidationId for containers summary");
        }
        if(shipmentId != null)
            return calculateContainerSummary(containerDao.findByShipmentId(shipmentId));
        return calculateContainerSummary(containerDao.findByConsolidationId(consolidationId));
    }

    public ContainerSummaryResponse calculateContainerSummary(List<Containers> containersList) throws RunnerException {
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
        if(!isStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
            toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
        if(!isStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
            toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
        String finalToWeightUnit = toWeightUnit;
        if(containersList != null) {
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
            List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerIdIn(containersList.stream().map(BaseEntity::getId).toList());
            assignedContainers = shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getContainerId).distinct().count();
            groupedContainerSummaryList = new ArrayList<>(summaryMap.values());
        }
        ContainerSummaryResponse response = new ContainerSummaryResponse();
        response.setTotalPackages(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalPacks), 0, v1TenantSettingsResponse));
        response.setTotalContainers(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalContainerCount), 0, v1TenantSettingsResponse));
        response.setTotalWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalTareWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(tareWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalNetWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(netWeight), v1TenantSettingsResponse), toWeightUnit));
        response.setTotalContainerVolume(String.format(Constants.STRING_FORMAT, IReport.convertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), v1TenantSettingsResponse), toVolumeUnit));
        if(response.getSummary() == null)
            response.setSummary("");
        setContainerSummary(containersList, response);
        response.setDgContainers(dgContainers);
        response.setAssignedContainersCount(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(assignedContainers), 0, v1TenantSettingsResponse));
        response.setGroupedContainersSummary(groupedContainerSummaryList);
        return response;
    }

    private void setContainerSummary(List<Containers> containersList, ContainerSummaryResponse response) {
        try {
            response.setSummary(getContainerSummary(containersList));
        }
        catch (Exception e) {
            log.error("Error calculating summary");
        }
    }

    private double getTotalPacks(Containers containers, double totalPacks) {
        if(!isStringNullOrEmpty(containers.getPacks()))
            totalPacks = totalPacks + Long.parseLong(containers.getPacks());
        return totalPacks;
    }

    private double getTotalContainerCount(Containers containers, double totalContainerCount) {
        if(containers.getContainerCount() != null)
            totalContainerCount = totalContainerCount + containers.getContainerCount();
        return totalContainerCount;
    }

    private int getDgContainers(Containers containers, int dgContainers) {
        if(Boolean.TRUE.equals(containers.getHazardous()))
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

    private ContainerNumberCheckResponse validateContainerNumberFormat(String containerNumber, ContainerNumberCheckResponse response) {
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
        try {
            List<ShipmentsContainersMapping> mappings;
            List<Containers> result = new ArrayList<>();
            if (request.getShipmentId() != null) {
                ShipmentDetails shipmentDetails = shipmentDao.findById(Long.valueOf(request.getShipmentId())).get();
                request.setTransportMode(shipmentDetails.getTransportMode());
                request.setExport(shipmentDetails.getDirection() != null && shipmentDetails.getDirection().equalsIgnoreCase(Constants.DIRECTION_EXP));
                mappings = shipmentsContainersMappingDao.findByShipmentId(Long.valueOf(request.getShipmentId()));
                List<Long> containerId = new ArrayList<>(mappings.stream().map(ShipmentsContainersMapping::getContainerId).toList());

                ListCommonRequest req = constructListCommonRequest("id", containerId, "IN");
                Pair<Specification<Containers>, Pageable> pair = fetchData(req, Containers.class);
                Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
                List<Containers> containersList = containers.getContent();
                result.addAll(containersList);
            }

            if (request.getConsolidationId() != null) {
                ConsolidationDetails consolidationDetails = consolidationDetailsDao.findById(Long.valueOf(request.getConsolidationId())).get();
                request.setTransportMode(consolidationDetails.getTransportMode());
                request.setExport(consolidationDetails.getShipmentType() != null && consolidationDetails.getShipmentType().equalsIgnoreCase(Constants.DIRECTION_EXP));
                ListCommonRequest req2 = constructListCommonRequest(Constants.CONSOLIDATION_ID, Long.valueOf(request.getConsolidationId()), "=");
                Pair<Specification<Containers>, Pageable> pair = fetchData(req2, Containers.class);
                Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
                List<Containers> containersList = containers.getContent();
                if (result.isEmpty()) {
                    result.addAll(containersList);
                } else {
                    result = result.stream().filter(result::contains).toList();
                }
            }
            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Containers_" + timestamp + Constants.XLSX;

            try(XSSFWorkbook workbook = new XSSFWorkbook()) {
                XSSFSheet sheet = workbook.createSheet("Containers");

                List<ContainersExcelModel> model = commonUtils.convertToList(result, ContainersExcelModel.class);
                convertModelToExcel(model, sheet, request);

                response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
                response.setHeader(Constants.CONTENT_DISPOSITION, Constants.ATTACHMENT_FILENAME + filenameWithTimestamp);

                try (OutputStream outputStream = response.getOutputStream()) {
                    workbook.write(outputStream);
                }
            }

        } catch (Exception ex) {
            throw new RunnerException(ex.getMessage());
        }

    }

    private void convertModelToExcel(List<ContainersExcelModel> modelList, XSSFSheet sheet, BulkDownloadRequest request) throws IllegalAccessException {

        // Create header row using annotations for order
        Row headerRow = sheet.createRow(0);
        Field[] fields = ContainersExcelModel.class.getDeclaredFields();

        Map<String, Field> fieldNameMap = Arrays.stream(fields).filter(f->f.isAnnotationPresent(ExcelCell.class)).collect(Collectors.toMap(Field::getName, c-> c));

        if(!Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && fieldNameMap.containsKey("containerStuffingLocation")) {
            Set<String> unlocationsRefGuids = new HashSet<>();
            processUnlocationsRefGuid(modelList, unlocationsRefGuids);
        }
        List<Field> fieldsList = new ArrayList<>();
        if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA) || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_ROA)
                || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_RF) || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_RAI))
            fieldsList = reorderFields(fieldNameMap, columnsSequenceForExcelDownload);
        else if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR))
            fieldsList = reorderFields(fieldNameMap, columnsSequenceForExcelDownloadForAir);
        int i = 0;
        for (var field : fieldsList){
            Cell cell = headerRow.createCell(i++);
            cell.setCellValue(!field.getAnnotation(ExcelCell.class).displayName().isEmpty() ? field.getAnnotation(ExcelCell.class).displayName() : field.getName());
        }

        // Populate data
        int rowIndex = 1;
        for (ContainersExcelModel model : modelList) {
            Row row = sheet.createRow(rowIndex++);
            int cellIndex = 0;
            for (Field field : fieldsList) {
                field.setAccessible(true);
                Object value = field.get(model);
                Cell cell = row.createCell(cellIndex++);
                cell.setCellValue(value != null ? value.toString() : "");
            }
        }
    }

    private void processUnlocationsRefGuid(List<ContainersExcelModel> modelList, Set<String> unlocationsRefGuids) {
        for (ContainersExcelModel model : modelList){
            unlocationsRefGuids.add(model.getContainerStuffingLocation());
        }
        if(!unlocationsRefGuids.isEmpty()) {
            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(unlocationsRefGuids, EntityTransferConstants.LOCATION_SERVICE_GUID);
            for (ContainersExcelModel model : modelList){
                if(keyMasterDataMap.containsKey(model.getContainerStuffingLocation())){
                    var locCode = keyMasterDataMap.get(model.getContainerStuffingLocation()).LocCode;
                    model.setContainerStuffingLocation(locCode);
                }
            }
        }
    }

    private List<Field> reorderFields(Map<String, Field> fieldNameMap, List<String> columnsName) {
        List<Field> fields = new ArrayList<>();
        for(var field: columnsName){
            if(fieldNameMap.containsKey(field)) {
                fields.add(fieldNameMap.get(field));
                fieldNameMap.remove(field);
            }
        }
        fields.addAll(fieldNameMap.values());
        return fields;
    }

}
