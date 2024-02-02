package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Dto.KafkaResponse;
import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerAssignListRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerNumberCheckResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerPackADInShipmentRequest;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.request.*;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.JobResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.impl.SyncEntityConversionService;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainerSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.StringUtility.isNotEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
public class ContainerService implements IContainerService {

    @Autowired
    IContainerDao containerDao;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    IShipmentsContainersMappingDao shipmentsContainersMappingDao;
    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private ExcelUtils excelUtils;

    @Autowired
    @Lazy
    private ConsolidationService consolidationService;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private CSVParsingUtil<Containers> parser;

    @Autowired
    private CSVParsingUtil<Events> newParser;
    @Autowired
    IEventDao eventDao;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private IPackingDao packingDao;
    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IContainerSync containerSync;

    @Autowired
    private IAuditLogService auditLogService;
    @Autowired
    private ICustomerBookingDao customerBookingDao;

    @Autowired
    private KafkaProducer producer;

    @Value("${containersKafka.queue}")
    private String senderQueue;
    @Lazy
    @Autowired
    private ISyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    IContainersSync containersSync;

    @Autowired
    IPackingsSync packingsADSync;
    private List<String> columnsSequenceForExcelDownload = List.of(
            "guid", "isOwnContainer", "isShipperOwned", "ownType", "isEmpty", "isReefer", "containerCode",
            "hblDeliveryMode", "containerNumber", "containerCount", "descriptionOfGoods", "handlingInfo", "noOfPackages",
            "hazardous", "dgClass", "hazardousUn", "packs", "packsType", "marksNums", "minTemp", "minTempUnit",
            "netWeight", "netWeightUnit", "grossWeight", "grossWeightUnit", "tareWeight", "tareWeightUnit", "grossVolume",
            "grossVolumeUnit", "measurement", "measurementUnit", "commodityCode", "hsCode", "customsReleaseCode",
            "containerStuffingLocation", "pacrNumber", "containerComments", "sealNumber", "carrierSealNumber",
            "shipperSealNumber", "terminalOperatorSealNumber", "veterinarySealNumber", "customsSealNumber"
    );
    private List<String> columnsSequenceForExcelDownloadForAir = List.of(
            "guid", "hblDeliveryMode", "descriptionOfGoods", "hazardous", "hazardousUn", "packs", "packsType",
            "marksNums", "serialNumber", "innerPackageNumber", "innerPackageType", "packageLength", "packageBreadth",
            "packageHeight", "innerPackageMeasurementUnit", "isTemperatureMaintained", "minTemp", "minTempUnit",
            "netWeight", "netWeightUnit", "grossWeight", "grossWeightUnit", "tareWeight", "tareWeightUnit", "chargeable",
            "chargeableUnit", "grossVolume", "grossVolumeUnit", "commodityCode", "hsCode", "customsReleaseCode", "pacrNumber",
            "containerComments"
    );

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Container Create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Containers container = convertRequestToEntity(request);
        List<EventsRequest> eventsRequestList = request.getEventsList();
        try {
            container = containerDao.save(container);
            if (request.getPacksList() != null) {
                List<PackingRequest> packingRequest = request.getPacksList();
                List<Packing> packs = packingDao.savePacks(convertToEntityList(packingRequest, Packing.class), container.getId());
                container.setPacksList(packs);
            }
            if (eventsRequestList != null) {
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), container.getId(), Constants.CONTAINER);
                container.setEventsList(events);
            }

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(container)
                            .prevData(null)
                            .parent(Containers.class.getSimpleName())
                            .parentId(container.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("Container Details Saved Successfully for Id {} with Request Id {}", container.getId(), LoggerHelper.getRequestIdFromMDC());
            afterSave(container, true);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
    }

    @Override
    public void uploadContainers(BulkUploadRequest request) throws Exception {
        List<Containers> consolContainers = containerDao.findByConsolidationId(request.getConsolidationId());
        var containerMap = consolContainers.stream().collect(Collectors.toMap(Containers::getGuid, Function.identity()));

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        List<Containers> containersList = parser.parseExcelFile(request.getFile(), request, containerMap, masterDataMap, Containers.class, ContainersExcelModel.class);

        containersList = containersList.stream().map(c ->
                c.setConsolidationId(request.getConsolidationId())
        ).collect(Collectors.toList());

        applyContainerValidations(containersList, request, masterDataMap);
        containersList = containerDao.saveAll(containersList);
        if (request.getShipmentId() != null) {
            containersList.stream().forEach(container -> {
                shipmentsContainersMappingDao.updateShipmentsMappings(container.getId(), List.of(request.getShipmentId()));
            });
        }
        containerSync.sync(containersList, request.getConsolidationId(), request.getShipmentId());
        afterSaveList(containersList, true);
    }

    private void applyContainerValidations(List<Containers> containersList, BulkUploadRequest request,
                                           Map<String, Set<String>> masterDataMap) throws Exception {
        String transportMode = request.getTransportMode();
        Set<String> dicCommodityType = masterDataMap.get("CommodityCodes");
        Set<String> dicLocType = masterDataMap.get("Unlocations");
        Set<String> hazardousClassMasterData = masterDataMap.get(MasterDataType.DG_CLASS.getDescription());
        for (int row = 0; row < containersList.size(); row++) {
            // Update scenario to be implemented here
            Containers containersRow = containersList.get(row);
            if (containersRow.getIsOwnContainer() != null && containersRow.getIsShipperOwned() != null
                    && containersRow.getIsOwnContainer() == true && containersRow.getIsShipperOwned() == true) {
                String errorMessagePart1 = "Multiple container ownership is selected at row ";
                String errorMessagePart2 = " - Kindly change and try re-uploading.";
                throw new ValidationException(errorMessagePart1 + (row + 1) + errorMessagePart2);
            }
            checkCalculatedVolumeAndActualVolume(request, row + 1, containersRow);
            applyContainerNumberValidation(transportMode, row + 1, containersRow);
            applyConatinerCountValidation(request, transportMode, row + 1, containersRow);
            applyChargeableValidation(transportMode, row, containersRow);
            checkForHandlingInfo(transportMode, row, containersRow);
            applyCommodityTypeValidation(dicCommodityType, row, containersRow);
            applyContainerStuffingValidation(dicLocType, row, containersRow);
            applyHazardousValidation(hazardousClassMasterData, row, containersRow);
            //TODO :: Add own type validation in future after cms integration
            isPartValidation(request, containersRow);
        }
    }

    private void isPartValidation(BulkUploadRequest request, Containers containersRow) {
        Boolean isPartValue = containersRow.getIsPart() == null ? false : containersRow.getIsPart();
        if (isPartValue) {
            var shipmentRecordOpt = shipmentDao.findById(request.getShipmentId());
            if (shipmentRecordOpt.isPresent() && Constants.CARGO_TYPE_FCL.equals(shipmentRecordOpt.get().getShipmentType()) && isPartValue) {
                throw new ValidationException("Shipment cargo type is FCL, Part containers are not allowed to be attached with FCL Shipment");
            }
        }
    }

    private static void applyContainerStuffingValidation(Set<String> dicLocType, int row, Containers containersRow) {
        if (containersRow.getContainerStuffingLocation() != null) {
            var containerStuffingLocation = containersRow.getContainerStuffingLocation().trim();
            if (!containerStuffingLocation.isEmpty()) {
                if (dicLocType != null && dicLocType.contains(containerStuffingLocation) == false) {
                    throw new ValidationException("Container Stuffing Location " + containerStuffingLocation + " is not valid at row " + row);
                }
            }
        }
    }

    private static void applyHazardousValidation(Set<String> hazardousClassMasterData, int row, Containers containersRow) {
        Boolean isHazardous = containersRow.getHazardous();
        if (isHazardous != null) {
            if (isHazardous == true) {
                // DG CLASS(HAZARDOUS CLASS)
                String dgClass = containersRow.getDgClass();
                if (!StringUtils.isEmpty(dgClass)) {
                    try {
                        if (hazardousClassMasterData != null && !hazardousClassMasterData.contains(dgClass)) {
                            throw new ValidationException("DG class is invalid at row: " + row);
                        }
                    } catch (Exception e) {
                        throw new ValidationException("DG class is invalid at row: " + row + ". Please provide correct DG class.");
                    }
                } else {
                    throw new ValidationException("DG class is empty at row: " + row + ". DG class is mandatory field for hazardous goods.");
                }

                //HAZARDOUS UN
                String hazardousUn = containersRow.getHazardousUn();
                if (!StringUtils.isEmpty(hazardousUn)) {
                    containersRow.setHazardousUn(hazardousUn);
                }
            }
        }
    }

    private static void applyCommodityTypeValidation(Set<String> dicCommodityType, int row, Containers containersRow) {
        String commodityCode = containersRow.getCommodityCode();
        commodityCode = commodityCode == null ? StringUtils.EMPTY : commodityCode.trim();
        if (!commodityCode.isEmpty()) {
            if (dicCommodityType != null && dicCommodityType.contains(commodityCode) == false) {
                throw new ValidationException("Commodity Type " + commodityCode + " is not valid at row " + row);
            }
        }
    }

    private static void checkForHandlingInfo(String transportMode, int row, Containers containersRow) {
        if (transportMode != null && transportMode.equals(Constants.TRANSPORT_MODE_AIR) && containersRow.getHandlingInfo() != null) {
            String handlingInfo = containersRow.getHandlingInfo();
            handlingInfo = handlingInfo.trim();
            if (handlingInfo.length() > 2500) {
                throw new ValidationException("Handling Information exceeds maximum allowed characters at row " + row + ".");
            }
        }
    }

    private void applyChargeableValidation(String transportMode, int row, Containers containersRow) throws Exception {
        if (containersRow.getChargeableUnit() != null && !containersRow.getChargeableUnit().isEmpty() &&
                transportMode != null && transportMode.equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR) &&
                containersRow.getChargeable() != null) {
            if (!containersRow.getChargeableUnit().equals(Constants.WEIGHT_UNIT_KG)) {
                throw new ValidationException("Chargeable unit not in KG at row: " + row);
            }
            var actualChargeable = containersRow.getChargeable();
            actualChargeable = actualChargeable.setScale(2, BigDecimal.ROUND_HALF_UP);
            BigDecimal calculatedChargeable = null;

            var vwob = consolidationService.calculateVolumeWeight(Constants.TRANSPORT_MODE_AIR,
                    containersRow.getGrossWeightUnit() == null ? Constants.WEIGHT_UNIT_KG : containersRow.getGrossWeightUnit(),
                    containersRow.getGrossVolumeUnit() == null ? Constants.VOLUME_UNIT_M3 : containersRow.getGrossVolumeUnit(),
                    containersRow.getGrossWeight() == null ? BigDecimal.ZERO : containersRow.getGrossWeight(),
                    containersRow.getGrossVolume() == null ? BigDecimal.ZERO : containersRow.getGrossVolume());
            if (vwob.getChargeable() != null) {
                calculatedChargeable = vwob.getChargeable();
                calculatedChargeable = calculatedChargeable.setScale(2, BigDecimal.ROUND_HALF_UP);
                if (calculatedChargeable != actualChargeable) {
                    throw new ValidationException("Chargeable is invalid at row: " + row);
                }
            }
        }
    }

    private void checkCalculatedVolumeAndActualVolume(BulkUploadRequest request, int row, Containers containersRow) {
        if (request.getTransportMode() != null && !StringUtility.isEmpty(request.getTransportMode()) && request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && containersRow.getGrossVolume() != null) {
            if (!containersRow.getGrossVolumeUnit().equals(Constants.VOLUME_UNIT_M3)) {
                throw new ValidationException("Gross Volume unit not in M3 at row: " + (row + 1));
            }
            BigDecimal actualVolume = containersRow.getGrossVolume();
            actualVolume = actualVolume.setScale(2, BigDecimal.ROUND_HALF_UP);
            BigDecimal calculatedVolume = getCalculatedVolume(containersRow.getPackageBreadth(), containersRow.getPackageLength(), containersRow.getPackageHeight());
            calculatedVolume = calculatedVolume.setScale(2, BigDecimal.ROUND_HALF_UP);
            if (calculatedVolume != null && actualVolume != calculatedVolume) {
                throw new ValidationException("Gross Volume is invalid at row: " + (row + 1));
            }
        } else if (request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && containersRow.getGrossVolume() != null
                && StringUtils.isEmpty(containersRow.getGrossVolumeUnit())) {
            throw new ValidationException("Gross Volume unit is empty or Gross Volume unit not entered at row: " + row);
        }
    }

    private BigDecimal getCalculatedVolume(BigDecimal packageBreadth, BigDecimal packageLength, BigDecimal packageHeight) {
        if (packageBreadth == null || packageLength == null || packageHeight == null) {
            return null;
        }
        return packageLength.multiply(packageBreadth).multiply(packageHeight);
    }

    private static void applyConatinerCountValidation(BulkUploadRequest request, String transportMode, int row, Containers containersRow) {
        if (transportMode != null && !transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {
            try {
                var containerCount = Integer.parseInt(String.valueOf(containersRow.getContainerCount()));
                if (containerCount < 1) {
                    throw new ValidationException("Container Count is not valid at row: " + row);
                }
            } catch (NumberFormatException e) {
                throw new ValidationException("Container Count is not valid at row: " + row + ". Please provide an integer value within the range of integer.");
            }
        }
    }

    private static void applyContainerNumberValidation(String transportMode, int row, Containers containersRow) {
        if (transportMode != null && !transportMode.equals(Constants.TRANSPORT_MODE_AIR)) {

            var containerCount = containersRow.getContainerCount();

            if (!StringUtils.isEmpty(containersRow.getContainerNumber()) && containerCount > 1) {
                throw new ValidationException("Container Number cannot be assigned if container count is greater than 1 at row: " + row);
            }

            if (StringUtils.isEmpty(containersRow.getContainerNumber())) {
                containersRow.setContainerNumber(null);
            }
        }
    }

    @Override
    public void uploadContainerEvents(BulkUploadRequest request) throws Exception {
//        CSVParsingUtil<Events> newParser = new CSVParsingUtil<>(Events.class);
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        List<Events> eventsList = newParser.parseExcelFile(request.getFile(), request, null, masterDataMap, Events.class, ContainerEventExcelModel.class);
        eventsList = eventsList.stream().map(c -> {
            c.setEntityId(request.getConsolidationId());
            c.setEntityType("CONSOLIDATION");
            return c;
        }).collect(Collectors.toList());
        eventsList = eventDao.saveAll(eventsList);
        // TODO- revisit Abhimanyu and handle sync as well
//        if (request.getShipmentId() != null) {
//            eventsList.stream().forEach(container -> {
//                shipmentsContainersMappingDao.updateShipmentsMappings(container.getId(), List.of(request.getShipmentId()));
//            });
//        }
    }

    @Override
    public void downloadContainers(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) throws Exception {
        try {
            List<ShipmentsContainersMapping> mappings;
            List<Containers> result = new ArrayList<>();
            if (request.getShipmentId() != null) {
                List<Long> containerId = new ArrayList<>();
                mappings = shipmentsContainersMappingDao.findByShipmentId(Long.valueOf(request.getShipmentId()));
                containerId.addAll(mappings.stream().map(mapping -> mapping.getContainerId()).collect(Collectors.toList()));

                ListCommonRequest req = constructListCommonRequest("id", containerId, "IN");
                Pair<Specification<Containers>, Pageable> pair = fetchData(req, Containers.class);
                Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
                List<Containers> containersList = containers.getContent();
                result.addAll(containersList);
            }

            if (request.getConsolidationId() != null) {
                ListCommonRequest req2 = constructListCommonRequest("consolidationId", Long.valueOf(request.getConsolidationId()), "=");
                Pair<Specification<Containers>, Pageable> pair = fetchData(req2, Containers.class);
                Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
                List<Containers> containersList = containers.getContent();
                if (result.isEmpty()) {
                    result.addAll(containersList);
                } else {
                    result = result.stream().filter(result::contains).collect(Collectors.toList());
                }
            }
            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "Containers_" + timestamp + ".xlsx";

            XSSFWorkbook workbook = new XSSFWorkbook();
            XSSFSheet sheet = workbook.createSheet("Containers");

            List<ContainersExcelModel> model = commonUtils.convertToList(result, ContainersExcelModel.class);
            convertModelToExcel(model, sheet, request);

            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

            try (OutputStream outputStream = response.getOutputStream()) {
                workbook.write(outputStream);
            }

        } catch (Exception ex) {
            throw new RunnerException(ex.getMessage());
        }

    }
    private void convertModelToExcel(List<ContainersExcelModel> modelList, XSSFSheet sheet, BulkDownloadRequest request) throws IllegalAccessException {

        // Create header row using annotations for order
        Row headerRow = sheet.createRow(0);
        Field[] fields = ContainersExcelModel.class.getDeclaredFields();
//        Arrays.sort(fields, Comparator.comparingInt(f -> f.getAnnotation(ExcelCell.class).order()));

        Map<String, Field> fieldNameMap = Arrays.stream(fields).filter(f->f.isAnnotationPresent(ExcelCell.class)).collect(Collectors.toMap(Field::getName, c-> c));
        ColumnsToIgnore(fieldNameMap, request);

        if(!Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR) && fieldNameMap.containsKey("containerStuffingLocation")) {
            List<String> unlocationsRefGuids = new ArrayList<>();
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

    private void ColumnsToIgnore(Map<String, Field> fieldNameMap, BulkDownloadRequest request) {
        if(request.getIsExport()){
            for(var field : Constants.ColumnsToBeDeletedForExport) {
                if (fieldNameMap.containsKey(field)) {
                    fieldNameMap.remove(field);
                }
            }
        } else {
            for (var field : Constants.ColumnsToBeDeleted) {
                if (fieldNameMap.containsKey(field)) {
                    fieldNameMap.remove(field);
                }
            }

            if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)){
                for (var field : Constants.ColumnsToBeDeletedForCargo) {
                    if (fieldNameMap.containsKey(field)) {
                        fieldNameMap.remove(field);
                    }
                }
            } else if(Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_SEA) || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_ROA)
            || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_RF) || Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_RAI)) {
                for (var field : Constants.ColumnsToBeDeletedForContainer) {
                    if (fieldNameMap.containsKey(field)) {
                        fieldNameMap.remove(field);
                    }
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

    @Override
    public void downloadContainerEvents(HttpServletResponse response, BulkDownloadRequest request) throws Exception {
        List<ContainerEventExcelModel> eventsModelList = new ArrayList<>();
        if (request.getConsolidationId() != null) {

            ListCommonRequest req = constructListCommonRequest("consolidationId", Long.valueOf(request.getConsolidationId()), "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(req, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            List<Containers> containersList = containers.getContent();

            if(!containersList.isEmpty()) {
                for (var container : containersList) {
                    List<Events> events = container.getEventsList();
                    List<ContainerEventExcelModel> modelList = commonUtils.convertToList(events, ContainerEventExcelModel.class);
                    modelList.forEach(x -> x.setContainerNumber(!Objects.isNull(container.getContainerNumber()) ? container.getContainerNumber() : ""));
                    eventsModelList.addAll(modelList);
                }
            }
        }
        LocalDateTime currentTime = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        String timestamp = currentTime.format(formatter);
        String filenameWithTimestamp = "Containers_Events_" + timestamp + ".xlsx";

        XSSFWorkbook workbook = new XSSFWorkbook();
        XSSFSheet sheet = workbook.createSheet("Containers_Events");
        convertModelToExcelForContainersEvent(eventsModelList, sheet, request);

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

        try (OutputStream outputStream = response.getOutputStream()) {
            workbook.write(outputStream);
        }
    }
    private void convertModelToExcelForContainersEvent(List<ContainerEventExcelModel> modelList, XSSFSheet sheet, BulkDownloadRequest request) throws IllegalAccessException {

        // Create header row using annotations for order
        Row headerRow = sheet.createRow(0);
        Field[] fields = ContainerEventExcelModel.class.getDeclaredFields();

        Map<String, Field> fieldNameMap = Arrays.stream(fields).filter(f->f.isAnnotationPresent(ExcelCell.class)).collect(Collectors.toMap(Field::getName, c-> c));
        List<Field> fieldsList = new ArrayList<>(fieldNameMap.values().stream().toList());
        fieldsList.sort(Comparator.comparingInt(f -> f.getAnnotation(ExcelCell.class).order()));
        int actualFieldIdx = -1;
        int estimatedFieldIdx = -1;
        int i = 0;
        for (var field : fieldsList){
            if(Objects.equals(field.getName(), "actual"))
                actualFieldIdx = i;
            if(Objects.equals(field.getName(), "estimated"))
                estimatedFieldIdx = i;
            Cell cell = headerRow.createCell(i++);
            cell.setCellValue(!field.getAnnotation(ExcelCell.class).displayName().isEmpty() ? field.getAnnotation(ExcelCell.class).displayName() : field.getName());
        }
        String format = "MM/dd/yyyy hh:mm:ss a";
        // Populate data
        int rowIndex = 1;
        for (ContainerEventExcelModel model : modelList) {
            Row row = sheet.createRow(rowIndex++);
            int cellIndex = 0;
            for (Field field : fieldsList) {
                field.setAccessible(true);
                Object value = field.get(model);
                if(actualFieldIdx != -1 && cellIndex == actualFieldIdx){
                    assert value instanceof LocalDateTime;
                    value = ReportHelper.GenerateFormattedDate((LocalDateTime) value, format);
                }
                if(estimatedFieldIdx != -1 && cellIndex == estimatedFieldIdx){
                    assert value instanceof LocalDateTime;
                    value = ReportHelper.GenerateFormattedDate((LocalDateTime) value, format);
                }
                Cell cell = row.createCell(cellIndex++);
                cell.setCellValue(value != null ? value.toString() : "");
            }
        }
    }


    @Transactional
    public ResponseEntity<?> attachPacks(Long containerId, List<Long> packsId) {
        Containers containers = containerDao.findById(containerId).get();

        if (containers != null) {
            for (Long packid : packsId) {
                Packing packing = packingDao.findById(packid).get();
                if (packing != null) {
                    containers.getPacksList().add(packing);
                }
            }
            Containers entity = containerDao.save(containers);
            afterSave(entity, false);
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ContainerResponse.class));
        }

        return null;
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ContainerRequest request = (ContainerRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Container Update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Containers> oldEntity = containerDao.findById(id);
        List<Long> updatedPackIds = new ArrayList<>();
        List<PackingRequest> updatedPackingRequest = new ArrayList<>();
        List<PackingRequest> packingRequestList = request.getPacksList();
        if(packingRequestList != null && !packingRequestList.isEmpty()) {
            for(PackingRequest packingRequest : packingRequestList) {
                if(packingRequest.getId() != null) {
                    updatedPackIds.add(packingRequest.getId());
                }
            }
        }

        List<PackingRequest> packingRequestWithEmptyContainerId = new ArrayList<>();
        if(packingRequestList != null && !packingRequestList.isEmpty()) {
            for(PackingRequest packingRequest : packingRequestList) {
                if(packingRequest.getContainerId() == null) {
                    packingRequestWithEmptyContainerId.add(packingRequest);
                }
            }
        }

        request.setPacksList(updatedPackingRequest);



        Containers containers = convertRequestToEntity(request);

        if(containers.getGuid() != null && !oldEntity.get().getGuid().equals(containers.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        List<EventsRequest> eventsRequestList = request.getEventsList();
        try {

            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            containers = containerDao.save(containers);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(containers)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Containers.class))
                            .parent(Containers.class.getSimpleName())
                            .parentId(containers.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            if (packingRequestList != null) {
                packingDao.removeContainerFromPacking(convertToEntityList(packingRequestList, Packing.class), id, updatedPackIds);
                packingDao.insertContainerInPacking(convertToEntityList(packingRequestWithEmptyContainerId, Packing.class), id);

            }
            if(eventsRequestList != null){
                List<Events> events = eventDao.saveEntityFromOtherEntity(
                        convertToEntityList(eventsRequestList, Events.class), containers.getId(), Constants.CONTAINER);
                containers.setEventsList(events);
            }
            afterSave(containers, false);
            log.info("Updated the container details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Containers List with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentsList", request.getId(), "CONTAINS");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(pair.getLeft(), pair.getRight());

            log.info("Container detail list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containersPage.getContent()),
                    containersPage.getTotalPages(),
                    containersPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Containers async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Container detail async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(
                    ResponseHelper
                            .buildListSuccessResponse(
                                    convertEntityListToDtoList(containersPage.getContent()),
                                    containersPage.getTotalPages(),
                                    containersPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        if (commonRequestModel == null) {
            log.debug("Request is empty for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        if (commonRequestModel.getId() == null) {
            log.debug("Request Id is null for Containers delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Long id = commonRequestModel.getId();
        Optional<Containers> container = containerDao.findById(id);
        if (container.isEmpty()) {
            log.debug("Container details are null for id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(Constants.NO_DATA);
        }
        try {
            packingDao.deleteEntityFromContainer(id);
            String oldEntityJsonString = jsonHelper.convertToJson(container.get());
            containerDao.delete(container.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Containers.class))
                            .parent(Containers.class.getSimpleName())
                            .parentId(container.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );

            log.info("Deleted container for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse();
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Container retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Containers> container = containerDao.findById(id);
            if (container.isEmpty()) {
                log.debug("Container is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Container detail fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            JobResponse response = (JobResponse) convertEntityToDto(container.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private Containers changeAchievedUnit(Containers container) throws Exception{
        try {
            if(!IsStringNullOrEmpty(container.getAchievedVolumeUnit()) && !IsStringNullOrEmpty(container.getAllocatedVolumeUnit()) && !container.getAchievedVolumeUnit().equals(container.getAllocatedVolumeUnit())) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());
                container.setAchievedVolume(val);
            }
            if(!IsStringNullOrEmpty(container.getAchievedWeightUnit()) && !IsStringNullOrEmpty(container.getAllocatedWeightUnit()) && !container.getAchievedWeightUnit().equals(container.getAllocatedWeightUnit())) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                container.setAchievedWeight(val);
            }
            container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
            container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
            container = calculateUtilization(container);
            return container;
        } catch (Exception e) {
            throw new Exception(e);
        }
    }

    @Override
    public Containers calculateUtilization(Containers container) {
        if(container.getAchievedVolume() == null)
            container.setAchievedVolume(BigDecimal.ZERO);
        if(container.getAchievedWeight() == null)
            container.setAchievedWeight(BigDecimal.ZERO);
        if(container.getAllocatedWeight() != null) {
            if(container.getAchievedWeight().compareTo(container.getAllocatedWeight()) > 0)
                container.setWeightUtilization("100");
            else if (Objects.equals(container.getAllocatedWeight(), BigDecimal.ZERO))
                container.setWeightUtilization("0");
            else
                container.setWeightUtilization( String.valueOf((container.getAchievedWeight().divide(container.getAllocatedWeight(), 4, BigDecimal.ROUND_HALF_UP)).multiply(new BigDecimal(100)).doubleValue()) );
        }
        else
            container.setWeightUtilization("0");
        if(container.getAllocatedVolume() != null) {
            if(container.getAchievedVolume().compareTo(container.getAllocatedVolume()) > 0)
                container.setVolumeUtilization("100");
            else if (Objects.equals(container.getAllocatedVolume(), BigDecimal.ZERO))
                container.setVolumeUtilization("0");
            else
                container.setVolumeUtilization( String.valueOf((container.getAchievedVolume().divide(container.getAllocatedVolume(), 4, BigDecimal.ROUND_HALF_UP)).multiply(new BigDecimal(100)).doubleValue()) );
        }
        else
            container.setVolumeUtilization("0");
        return container;
    }

    @Override
    public ResponseEntity<?> calculateAchieved_AllocatedForSameUnit(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerRequest containerRequest = (ContainerRequest) commonRequestModel.getData();
            Containers container = convertRequestToEntity(containerRequest);
            container = changeAchievedUnit(container);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(container));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

//    @Override
//    public ResponseEntity<?> calculateAchievedQuantity_onPackAssign(CommonRequestModel commonRequestModel) {
//        String responseMsg;
//        try {
//            ContainerPackADInShipmentRequest request = (ContainerPackADInShipmentRequest) commonRequestModel.getData();
//
//            Optional<Containers> containersOptional = containerDao.findById(request.getContainerId());
//            if(containersOptional.isPresent()) {
//                Containers container = containersOptional.get();
//                changeAchievedUnit(container);
//                ListCommonRequest listCommonRequest = constructListCommonRequest("id", request.getPacksId(), "IN");
//                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
//                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
//                if(!packings.isEmpty() && packings.get().findAny().isPresent()) {
//                    List<Packing> packingList = packings.stream().toList();
//                    for(Packing packing: packingList) {
//                        if(packing.getWeight() != null && !packing.getWeightUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedWeightUnit())) {
//                            BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
//                            container.setAchievedWeight(container.getAchievedWeight().add(val));
//                            container.setWeightUtilization(((container.getAchievedWeight().divide(container.getAllocatedWeight())).multiply(new BigDecimal(100))).toString());
//                        }
//                        if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedVolumeUnit())) {
//                            BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
//                            container.setAchievedVolume(container.getAchievedVolume().add(val));
//                            container.setVolumeUtilization(((container.getAchievedVolume().divide(container.getAllocatedVolume())).multiply(new BigDecimal(100))).toString());
//                        }
//                    }
//                    return assignContainers(packingList, container, request.getShipmentId());
//                }
//            }
//            responseMsg = "Data not available for provided request";
//            throw new DataRetrievalFailureException(responseMsg);
//        } catch (Exception e) {
//            responseMsg = e.getMessage() != null ? e.getMessage()
//                    : DaoConstants.DAO_CALCULATION_ERROR;
//            log.error(responseMsg, e);
//            return ResponseHelper.buildFailedResponse(responseMsg);
//        }
//    }

//    public ResponseEntity<?> assignContainers(List<Packing> packingList, Containers container, Long shipmentId) {
//        String responseMsg;
//        try {
//            shipmentsContainersMappingDao.assignShipments(container.getId(), List.of(shipmentId));
//            Containers containers = containerDao.save(jsonHelper.convertValue(container, Containers.class));
//            for (Packing packing: packingList) {
//                packing.setContainerId(container.getId());
//            }
//            packingDao.saveAll(packingList);
//            afterSave(containers, false);
//            return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
//        } catch (Exception e) {
//            responseMsg = e.getMessage() != null ? e.getMessage()
//                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
//            log.error(responseMsg, e);
//            return ResponseHelper.buildFailedResponse(responseMsg);
//        }
//    }

    @Override
    public ResponseEntity<?> calculateAchievedQuantity_onPackDetach(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerPackADInShipmentRequest request = (ContainerPackADInShipmentRequest) commonRequestModel.getData();

            Optional<Containers> containersOptional = containerDao.findById(request.getContainerId());
            if(containersOptional.isPresent()) {
                Containers container = containersOptional.get();
                changeAchievedUnit(container);
                ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
                if(request.getPacksId() != null && request.getPacksId().size() > 0) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", request.getShipmentId(), "=");
                    Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                    Page<Packing> allPackings = packingDao.findAll(pair.getLeft(), pair.getRight());
                    if(!allPackings.isEmpty() && allPackings.get().findAny().isPresent())
                    {
                        List<Packing> packingList = allPackings.stream().toList();
                        List<Packing> detachPacks = new ArrayList<>();
                        boolean removeAllPacks = true;
                        for(Packing packing: packingList) {
                            if(request.getPacksId().contains(packing.getId())) {
                                detachPacks.add(packing);
                                if(!shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                                    if(packing.getWeight() != null && !packing.getWeightUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedWeightUnit())) {
                                        BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
                                        container.setAchievedWeight(container.getAchievedWeight().subtract(val));
                                    }
                                    if(packing.getVolume() != null && !packing.getVolumeUnit().isEmpty() && !IsStringNullOrEmpty(container.getAchievedVolumeUnit())) {
                                        BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
                                        container.setAchievedVolume(container.getAchievedVolume().subtract(val));
                                    }
                                    container = calculateUtilization(container);
                                }
                            }
                            else if(packing.getContainerId() != null && packing.getContainerId().equals(request.getContainerId()))
                                removeAllPacks = false;
                        }
                        if(removeAllPacks && shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                            container.setAchievedWeight(BigDecimal.ZERO);
                            container.setAchievedVolume(BigDecimal.ZERO);
                            container.setWeightUtilization("0");
                            container.setVolumeUtilization("0");
                        }
                        return detachContainer(detachPacks, container, request.getShipmentId(), removeAllPacks);
                    }
                }
                else {
                    if(shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        container.setAchievedWeight(BigDecimal.ZERO);
                        container.setAchievedVolume(BigDecimal.ZERO);
                        container.setWeightUtilization("0");
                        container.setVolumeUtilization("0");
                    }
                    return detachContainer(null, container, request.getShipmentId(), true);
                }
            }
            responseMsg = "Data not available for provided request";
            throw new DataRetrievalFailureException(responseMsg);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> detachContainer(List<Packing> packingList, Containers container, Long shipmentId, boolean removeAllPacks) {
        String responseMsg;
        try {
            Containers containers = containerDao.save(container);
            if(removeAllPacks)
                shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipmentId), false);
            else {
                try {
                    log.info("Call sync containers from detachContainer with ids: " + containers.toString());
                    containersSync.sync(List.of(containers.getId()), shipmentsContainersMappingDao.findAllByContainerIds(List.of(containers.getId())));
                }
                catch (Exception e) {
                    log.error("Error syncing containers");
                }
            }
            if(packingList != null && packingList.size() > 0) {
                for (Packing packing: packingList) {
                    packing.setContainerId(null);
                }
                packingDao.saveAll(packingList);
                try {
                    packingsADSync.sync(packingList);
                }
                catch (Exception e) {
                    log.error("Error syncing packings");
                }
            }
            afterSave(containers, false);
            return ResponseHelper.buildSuccessResponse(convertEntityToDto(containers));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> getContainersForSelection(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
        boolean lclAndSeaOrRoadFlag = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean IsConsolidatorFlag = shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator();
        List<Containers> containersList = new ArrayList<>();
        try {
            ContainerAssignListRequest containerAssignRequest = (ContainerAssignListRequest) commonRequestModel.getData();
            Long shipmentId = containerAssignRequest.getShipmentId();
            Long consolidationId = containerAssignRequest.getConsolidationId();
            if (lclAndSeaOrRoadFlag) {
                if(!containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA)) {
                    lclAndSeaOrRoadFlag = false;
                }
            }
            ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            List<Containers> conts = new ArrayList<>();
            if(lclAndSeaOrRoadFlag) {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {

                        if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedVolume() != null && container.getAchievedWeight() != null
                           && isNotEmpty(container.getAllocatedWeightUnit()) && isNotEmpty(container.getAllocatedVolumeUnit()) && isNotEmpty(container.getAchievedWeightUnit()) && isNotEmpty(container.getAchievedVolumeUnit())) {

                            BigDecimal achievedWeight = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                            BigDecimal achievedVolume = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());

                            if(achievedWeight.compareTo(container.getAllocatedWeight()) < 0 && achievedVolume.compareTo(container.getAllocatedVolume()) < 0) {
                                containersList.add(container);
                            }
                            else if(!IsConsolidatorFlag) {
                                conts.add(container);
                            }
                        }
                        else
                            containersList.add(container);
                    }
                }
                if(conts.size() > 0) {
                    for (Containers x : conts) {
                        boolean flag = true;
                        if(x.getShipmentsList() != null && x.getShipmentsList().size() > 0) {
                            for(ShipmentDetails shipmentDetails : x.getShipmentsList()) {
                                if(shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL))
                                    flag = false;
                            }
                        }
                        if (flag)
                            containersList.add(x);
                    }
                }
            }
            else {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {
                        containersList.add(container);
                    }
                }
            }
            if(containerAssignRequest.getTake() != null)
                containers = new PageImpl<>(containersList.subList(0, Math.min(containerAssignRequest.getTake(), containersList.size())), PageRequest.of(0, containerAssignRequest.getTake()), containersList.size());
            else
                containers = new PageImpl<>(containersList);
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containers.getContent()),
                    containers.getTotalPages(),
                    containers.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> validateContainerNumber(String containerNumber) {
        String responseMsg;
        try {
            ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
            response.setLastDigit(-1);
            if (containerNumber.length() != 10 && containerNumber.length() != 11) {
                response.setSuccess(false);
                return ResponseHelper.buildSuccessResponse(response);
            }
            for (int i = 0; i < 4; i++) {
                if ((int) containerNumber.charAt(i) < 65 || (int) containerNumber.charAt(i) > 90) {
                    response.setSuccess(false);
                    return ResponseHelper.buildSuccessResponse(response);
                }
            }
            for (int i = 4; i < 10; i++) {
                if ((int) containerNumber.charAt(i) < 48 || (int) containerNumber.charAt(i) > 57) {
                    response.setSuccess(false);
                    return ResponseHelper.buildSuccessResponse(response);
                }
            }
            if (containerNumber.length() == 11) {
                if ((int) containerNumber.charAt(10) < 48 || (int) containerNumber.charAt(10) > 57) {
                    response.setSuccess(false);
                    return ResponseHelper.buildSuccessResponse(response);
                }
            }
            List<Integer> eqvNumValue = assignEquivalentNumberValue();
            int expandedVal = 0;
            for (int i = 0; i < 4; i++) {
                expandedVal = expandedVal + (int) Math.pow(2, i) * eqvNumValue.get((int) containerNumber.charAt(i) - 65);
            }
            for (int i = 4; i < 10; i++) {
                expandedVal = expandedVal + (int) Math.pow(2, i) * ((int) containerNumber.charAt(i) - 48);
            }
            int checkDigit = expandedVal % 11;
            if (checkDigit == 10)
                checkDigit = 0;
            if (containerNumber.length() == 11) {
                if (checkDigit != (int) containerNumber.charAt(10) - 48) {
                    response.setSuccess(false);
                    response.setLastDigit(-2);
                    return ResponseHelper.buildSuccessResponse(response);
                }
            } else response.setLastDigit(checkDigit);
            response.setSuccess(true);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_INVALID_REQUEST_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> getContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for container list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Event list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containersPage.getContent()),
                    containersPage.getTotalPages(),
                    containersPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }

    }

    @Override
    public ResponseEntity<?> checkForDelete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            Long id = commonRequestModel.getId();
            List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(id);
            if(shipmentsContainersMappings != null && shipmentsContainersMappings.size() > 1)
                return ResponseHelper.buildSuccessResponse(true);
            return ResponseHelper.buildSuccessResponse(false);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
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

    public ContainerSummaryResponse calculateContainerSummary(List<Containers> containersList, String transportMode, String containerCategory) throws Exception {
        try {
            double totalWeight = 0;
            double packageCount = 0;
            double tareWeight = 0;
            double totalVolume = 0;
            double totalContainerCount = 0;
            double totalPacks = 0;
            String toWeightUnit = Constants.WEIGHT_UNIT_KG;
            String toVolumeUnit = Constants.VOLUME_UNIT_M3;
            ShipmentSettingsDetails shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(TenantContext.getCurrentTenant())).get(0);
            if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
                toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
            if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
                toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
            if(containersList != null) {
                for (Containers containers : containersList) {
                    double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                    double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                    double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                    totalWeight = totalWeight + wInDef;
                    tareWeight = tareWeight + tarDef;
                    double noOfPackages = 0;
                    if(containers.getNoOfPackages() != null)
                        noOfPackages = containers.getNoOfPackages().doubleValue();
                    if(!IsStringNullOrEmpty(containers.getPacks()))
                        packageCount = packageCount + Long.parseLong(containers.getPacks());
                    else
                        packageCount = packageCount + noOfPackages;
                    totalVolume = totalVolume + volume;
                    if(containers.getContainerCount() != null)
                        totalContainerCount = totalContainerCount + containers.getContainerCount();
                    if(!IsStringNullOrEmpty(containers.getPacks()))
                        totalPacks = totalPacks + Long.parseLong(containers.getPacks());
                }
            }
            ContainerSummaryResponse response = new ContainerSummaryResponse();
            response.setTotalPackages(String.valueOf(packageCount));
            response.setTotalContainers(String.valueOf(totalContainerCount));
            response.setTotalWeight(String.format("%.2f %s", totalWeight, toWeightUnit));
            response.setTotalTareWeight(String.format("%.2f %s", tareWeight, toWeightUnit));
            if(!IsStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_SEA) &&
                    !IsStringNullOrEmpty(containerCategory) && containerCategory.equals(Constants.SHIPMENT_TYPE_LCL)) {
                double volInM3 = convertUnit(Constants.VOLUME, new BigDecimal(totalVolume), toVolumeUnit, Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, new BigDecimal(totalWeight), toWeightUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
                double chargeableWeight = Math.max(wtInKg/1000, volInM3);
                response.setChargeableWeight(chargeableWeight + " " + Constants.VOLUME_UNIT_M3);
            }
            response.setTotalContainerVolume(String.format("%.2f %s", totalVolume, toVolumeUnit));
            if(response.getSummary() == null)
                response.setSummary("");
            try {
                response.setSummary(calculateContainerSummary(containersList));
            }
            catch (Exception e) {
                log.error("Error calculating summary");
            }
            return response;
        }
        catch (Exception e) {
            throw new Exception(e);
        }
    }

    public String calculateContainerSummary(List<Containers> response) {
        if(response == null)
            response = new ArrayList<>();
        response.sort(Comparator.comparing(Containers::getContainerCode));

        StringBuilder summary = new StringBuilder();
        Long containerCountPart = 0L;
        Long containerCountNoPart = 0L;

        if (!response.isEmpty()) {
            for (int i = 0; i < response.size(); i++) {
                Containers container = response.get(i);
                if (container.getIsPart() != null && container.getIsPart()) {
                    containerCountPart = containerCountPart + container.getContainerCount();
                } else {
                    containerCountNoPart = containerCountNoPart + container.getContainerCount();
                }
            }

            if (containerCountNoPart > 0) {
                summary.append(" ").append(inWords(containerCountNoPart)).append("(");

                for (int i = 0; i < response.size(); i++) {
                    Containers container = response.get(i);
                    if (container.getIsPart() == null || !container.getIsPart()) {
                        Long containerCount = container.getContainerCount();
                        int j;
                        for (j = i + 1; j < response.size(); j++) {
                            Containers nextContainer = response.get(j);
                            if (nextContainer.getContainerCode().equals(container.getContainerCode())) {
                                if (!nextContainer.getIsPart()) {
                                    containerCount = containerCount + nextContainer.getContainerCount();
                                }
                            } else {
                                break;
                            }
                        }
                        summary.append(container.getContainerCode()).append(" * ").append(containerCount).append(", ");
                        i = j - 1;
                    }
                }
                summary = new StringBuilder(summary.substring(0, summary.length() - 2));
                summary.append("), ");
            }

            if (containerCountPart > 0) {
                summary.append(" ").append(inWords(containerCountPart)).append("( Part of ");

                for (int i = 0; i < response.size(); i++) {
                    Containers container = response.get(i);
                    if (container.getIsPart()) {
                        Long containerCount = container.getContainerCount();
                        int j;
                        for (j = i + 1; j < response.size(); j++) {
                            Containers nextContainer = response.get(j);
                            if (nextContainer.getContainerCode().equals(container.getContainerCode())) {
                                if (nextContainer.getIsPart()) {
                                    containerCount = containerCount + nextContainer.getContainerCount();
                                }
                            } else {
                                break;
                            }
                        }
                        summary.append(container.getContainerCode()).append(" * ").append(containerCount).append(", ");
                        i = j - 1;
                    }
                }
                summary = new StringBuilder(summary.substring(0, summary.length() - 2));
                summary.append(")");
            } else {
                summary = new StringBuilder(summary.substring(0, summary.length() - 2));
            }
            return summary.toString();
        }
        return null;
    }

    public void afterSave(Containers containers, boolean isCreate) {
        try {
            if(containers.getTenantId() == null)
                containers.setTenantId(TenantContext.getCurrentTenant());
            KafkaResponse kafkaResponse = producer.getKafkaResponse(containers, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        } catch (Exception e) {
            log.error("Error pushing container to kafka");
        }
    }

    public void afterSaveList(List<Containers> containers, boolean isCreate) {
        if(containers != null && containers.size() > 0) {
            for (Containers container : containers) {
                afterSave(container, isCreate);
            }
        }
    }

    /**
     * V1 -> V2 sync
     */
    
    @Override
    public ResponseEntity<?> V1ContainerCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception {
        ContainerRequestV2 containerRequest = (ContainerRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.CONTAINERS, StringUtility.convertToString(containerRequest.getGuid()), containerRequest);
            }
            List<Containers> existingCont = containerDao.findByGuid(containerRequest.getGuid());
            Containers containers = syncEntityConversionService.containerV1ToV2(containerRequest);
            List<Long> shipIds = null;
            boolean isCreate = true;
            if (existingCont != null && existingCont.size() > 0) {
                if (existingCont != null && existingCont.size() > 0) {
                    containers.setId(existingCont.get(0).getId());
                    containers.setConsolidationId(existingCont.get(0).getConsolidationId());
                } else {
                    if (containerRequest.getConsolidationGuid() != null) {
                        isCreate = false;
                    }
                }
            } else {
                if (containerRequest.getConsolidationGuid() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(containerRequest.getConsolidationGuid());
                    if (!consolidationDetails.isEmpty() && consolidationDetails.get() != null) {
                        containers.setConsolidationId(consolidationDetails.get().getId());
                    }
                }
                if (containerRequest.getShipmentGuids() != null && containerRequest.getShipmentGuids().size() > 0) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest("guid", containerRequest.getShipmentGuids(), "IN");
                    Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
                    Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
                    if (shipmentDetails.get() != null && shipmentDetails.get().count() > 0) {
                        shipIds = shipmentDetails.get().map(e -> e.getId()).collect(Collectors.toList());
                    }
                }
            }
            containers = containerDao.save(containers);
            afterSave(containers, isCreate);
            if (shipIds != null) {
                shipmentsContainersMappingDao.assignShipments(containers.getId(), shipIds, true);
            }
            ContainerResponse response = objectMapper.convertValue(containers, ContainerResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    /**
     * Create bulk containers from V1 in V2
     */
    @Override
    public ResponseEntity<?> V1BulkContainerCreateAndUpdate(CommonRequestModel commonRequestModel) {
        BulkContainerRequestV2 bulkContainerRequest = (BulkContainerRequestV2) commonRequestModel.getData();
        try {
            List<ResponseEntity<?>> responses = new ArrayList<>();
            for (ContainerRequestV2 containerRequest : bulkContainerRequest.getBulkContainers())
                responses.add(this.V1ContainerCreateAndUpdate(CommonRequestModel.builder()
                        .data(containerRequest)
                        .build(), true));
            return ResponseHelper.buildSuccessResponse(responses);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    @Override
    public void exportContainers(HttpServletResponse response, ExportContainerListRequest request) throws Exception {
        List<ShipmentsContainersMapping> mappings;
        Optional<ConsolidationDetails> consol = null;
        List<IRunnerResponse> containersList = null;
        if (request.getConsolidationId() != null) {
            consol = consolidationDetailsDao.findById(Long.valueOf(request.getConsolidationId()));

            if (consol.isEmpty())
                throw new RuntimeException("Consolidation does not exist, pls save the consol first");

            if (consol.get().getContainersList().isEmpty())
                throw new RuntimeException("No containers found attached to consoliation");

            List<Containers> containers = consol.get().getContainersList();
            if (containers == null || containers.isEmpty()) {
                throw new RuntimeException("No containers present for this consol");
            }
            containersList = convertEntityListToDtoList(containers);

        } else {
            throw new RuntimeException("Consolidation does not exist, pls save the consol first");
        }

        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("ContainersList");
        makeHeadersInSheet(sheet, consol);

        for (int i = 0; i < containersList.size(); i++) {
            Row itemRow = sheet.createRow(i + 1);
            ContainerResponse container = (ContainerResponse) containersList.get(i);
            var consolBasicValues = parser.getAllAttributeValuesAsListContainer(container);
            int offset = 0;
            for (int j = 0; j < consolBasicValues.size(); j++)
                itemRow.createCell(offset + j).setCellValue(consolBasicValues.get(j));
            offset += consolBasicValues.size();

            itemRow.createCell(offset + 0).setCellValue(consol.get().getBol());
            itemRow.createCell(offset + 1).setCellValue(0);
            itemRow.createCell(offset + 2).setCellValue(0);
            itemRow.createCell(offset + 3).setCellValue(request.getFreeTimeNoOfDaysDetention());
            itemRow.createCell(offset + 4).setCellValue(request.getFreeTimeNoOfDaysStorage());
            itemRow.createCell(offset + 5).setCellValue(consol.get().getCarrierDetails().getVoyage());

            var booking = customerBookingDao.findById(container.getBookingId());
            var bookingNum = booking.isPresent() ? booking.get().getBookingNumber() : "";
            var bookingDate = booking.isPresent() ? booking.get().getBookingDate() : null;

            itemRow.createCell(offset + 6).setCellValue(bookingDate);
            itemRow.createCell(offset + 7).setCellValue(bookingNum);
        }

        LocalDateTime currentTime = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss");
        String timestamp = currentTime.format(formatter);
        String filenameWithTimestamp = "ContainerList_" + timestamp + ".xlsx";

        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

        try (OutputStream outputStream = response.getOutputStream()) {
            workbook.write(outputStream);
        }

    }

    private void makeHeadersInSheet(Sheet sheet, Optional<ConsolidationDetails> consol) {
//        Row preHeaderRow = sheet.createRow(0);
        Row headerRow = sheet.createRow(0);
        List<String> containerHeader = parser.getHeadersForContainer();
        for (int i = 0; i < containerHeader.size(); i++) {
            Cell cell = headerRow.createCell(i);
            cell.setCellValue(containerHeader.get(i));
        }

        containerHeader.add("bol");
        containerHeader.add("NoOfDays (Detention)");
        containerHeader.add("NoOfDays (Storage)");
        containerHeader.add("FreeTimeNoOfDays (Storage)");
        containerHeader.add("FreeTimeNoOfDays (Detention)");
        containerHeader.add("Voyage");
        containerHeader.add("Booking Date");
        containerHeader.add("Booking Number");
    }


    private IRunnerResponse convertEntityToDto(Containers container) {
        return jsonHelper.convertValue(container, ContainerResponse.class);
    }

    private Containers convertRequestToEntity(ContainerRequest request) {
        return jsonHelper.convertValue(request, Containers.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Containers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(containers -> {
            responseList.add(convertEntityToDto(containers));
        });
        return responseList;
    }

}
