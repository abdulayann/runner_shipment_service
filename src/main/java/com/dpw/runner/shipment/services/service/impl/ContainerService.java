package com.dpw.runner.shipment.services.service.impl;

import com.azure.messaging.servicebus.ServiceBusMessage;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.ContainerEventExcelModel;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.GenericException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service_bus.ISBProperties;
import com.dpw.runner.shipment.services.service_bus.ISBUtils;
import com.dpw.runner.shipment.services.service_bus.model.ContainerBoomiUniversalJson;
import com.dpw.runner.shipment.services.service_bus.model.ContainerPayloadDetails;
import com.dpw.runner.shipment.services.service_bus.model.ContainerUpdateRequest;
import com.dpw.runner.shipment.services.service_bus.model.EventMessage;
import com.dpw.runner.shipment.services.syncing.Entity.BulkContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
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
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
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

    @Autowired
    private ISBUtils sbUtils;

    @Autowired
    private ISBProperties isbProperties;

    @Value("${boomi-message-topic}")
    private String messageTopic;


    @Value("${containersKafka.queue}")
    private String senderQueue;

    @Value("${containerTransportOrchestrator.queue}")
    private String transportOrchestratorQueue;

    @Autowired
    private SyncConfig syncConfig;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private SyncEntityConversionService syncEntityConversionService;

    @Autowired
    IContainersSync containersSync;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    IPackingsSync packingsADSync;
    private List<String> columnsSequenceForExcelDownload = List.of(
            "guid", "isOwnContainer", "isShipperOwned", "ownType", "isEmpty", "isReefer", "containerCode",
            "hblDeliveryMode", "containerNumber", "containerCount", "descriptionOfGoods", "handlingInfo",
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
    private List<String> defaultIncludeColumns = new ArrayList<>();
    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public void uploadContainers(BulkUploadRequest request) throws RunnerException, IOException {
        if (request == null || request.getConsolidationId() == null) {
            throw new ValidationException("Please add the consolidation and then try again.");
        }
        Map<UUID, Containers> containerMap;
        Map<String, UUID> containerNumbersSet;
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        List<Containers> consolContainers = containerDao.findByConsolidationId(request.getConsolidationId());
        containerMap = consolContainers.stream().filter(Objects::nonNull).collect(Collectors.toMap(Containers::getGuid, Function.identity()));
        containerNumbersSet = consolContainers.stream().filter(Objects::nonNull).filter(c -> c.getContainerNumber() != null).collect(Collectors.toMap(Containers::getContainerNumber, Containers::getGuid));

        Map<String, Set<String>> masterDataMap = new HashMap<>();
        List<Containers> containersList = parser.parseExcelFile(request.getFile(), request, containerMap, masterDataMap, Containers.class, ContainersExcelModel.class, null, null, locCodeToLocationReferenceGuidMap);

        containersList = containersList.stream().map(c ->
                c.setConsolidationId(request.getConsolidationId())
        ).toList();

        applyContainerValidations(containerNumbersSet, locCodeToLocationReferenceGuidMap, containersList, request, masterDataMap);
        containersList = containerDao.saveAll(containersList);
        if (request.getShipmentId() != null && !CommonUtils.listIsNullOrEmpty(containersList)) {
            containersList.forEach(container -> shipmentsContainersMappingDao.updateShipmentsMappings(container.getId(), List.of(request.getShipmentId())));
        }
        containerSync.sync(containersList, request.getConsolidationId(), request.getShipmentId());
        afterSaveList(containersList, true);
    }

    private void applyContainerValidations(Map<String, UUID> containerNumberSet, Map<String, String> locCodeToLocationReferenceGuidMap, List<Containers> containersList, BulkUploadRequest request,
                                           Map<String, Set<String>> masterDataMap) throws RunnerException {
        String transportMode = request.getTransportMode();
        Set<String> dicCommodityType = masterDataMap.get("CommodityCodes");
        Set<String> dicLocType = masterDataMap.get("Unlocations");
        Set<String> hazardousClassMasterData = masterDataMap.get(MasterDataType.DG_CLASS.getDescription());
        for (int row = 0; row < containersList.size(); row++) {
            Containers containersRow = containersList.get(row);
            if (Boolean.TRUE.equals(containersRow.getIsOwnContainer()) && Boolean.TRUE.equals(containersRow.getIsShipperOwned())) {
                String errorMessagePart1 = "Multiple container ownership is selected at row ";
                String errorMessagePart2 = " - Kindly change and try re-uploading.";
                throw new ValidationException(errorMessagePart1 + (row + 1) + errorMessagePart2);
            }
            if(containerNumberSet != null && !StringUtils.isEmpty(containersRow.getContainerNumber())
                    && containerNumberSet.containsKey(containersRow.getContainerNumber()) && !Objects.equals(containersRow.getGuid(), containerNumberSet.get(containersRow.getContainerNumber())))
                throw new ValidationException("Container Number already exists, please check container number at row : " + row + 1);

            checkCalculatedVolumeAndActualVolume(request, row + 1, containersRow);
            applyContainerNumberValidation(transportMode, row + 1, containersRow);
            applyContainerCountValidation(transportMode, row + 1, containersRow);
            applyChargeableValidation(transportMode, row + 1, containersRow);
            checkForHandlingInfo(transportMode, row + 1, containersRow);
            applyCommodityTypeValidation(dicCommodityType, row + 1, containersRow);
            applyContainerStuffingValidation(dicLocType, locCodeToLocationReferenceGuidMap, row + 1, containersRow);
            applyHazardousValidation(hazardousClassMasterData, row + 1, containersRow);
            //LATER :: Add own type validation in future after cms integration
            isPartValidation(request, containersRow);
        }
    }

    private void isPartValidation(BulkUploadRequest request, Containers containersRow) {
        boolean isPartValue = containersRow.getIsPart() != null && containersRow.getIsPart();
        if (isPartValue) {
            var shipmentRecordOpt = shipmentDao.findById(request.getShipmentId());
            if (shipmentRecordOpt.isPresent() && Constants.CARGO_TYPE_FCL.equals(shipmentRecordOpt.get().getShipmentType()) && isPartValue) {
                throw new ValidationException("Shipment cargo type is FCL, Part containers are not allowed to be attached with FCL Shipment");
            }
        }
    }

    private static void applyContainerStuffingValidation(Set<String> dicLocType, Map<String, String> locCodeToLocationReferenceGuidMap, int row, Containers containersRow) {
        if (containersRow.getContainerStuffingLocation() != null) {
            var containerStuffingLocation = containersRow.getContainerStuffingLocation().trim();
            if (!containerStuffingLocation.isEmpty()) {
                if (dicLocType == null || !dicLocType.contains(containerStuffingLocation)) {
                    throw new ValidationException("Container Stuffing Location " + containerStuffingLocation + " is not valid at row " + row);
                }else {
                    containersRow.setContainerStuffingLocation(locCodeToLocationReferenceGuidMap.get(containerStuffingLocation));
                }
            }
        }
    }

    private static void applyHazardousValidation(Set<String> hazardousClassMasterData, int row, Containers containersRow) {
        Boolean isHazardous = containersRow.getHazardous();
        if (isHazardous != null && isHazardous) {
            // DG CLASS(HAZARDOUS CLASS)
            String dgClass = containersRow.getDgClass();
            validateDgClass(hazardousClassMasterData, row, dgClass);

            //HAZARDOUS UN
            String hazardousUn = containersRow.getHazardousUn();
            if (!StringUtils.isEmpty(hazardousUn)) {
                containersRow.setHazardousUn(hazardousUn);
            }
        }
    }

    private static void validateDgClass(Set<String> hazardousClassMasterData, int row, String dgClass) {
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
    }

    private static void applyCommodityTypeValidation(Set<String> dicCommodityType, int row, Containers containersRow) {
        String commodityCode = containersRow.getCommodityCode();
        commodityCode = commodityCode == null ? StringUtils.EMPTY : commodityCode.trim();
        if (!commodityCode.isEmpty() && (dicCommodityType == null || !dicCommodityType.contains(commodityCode))) {
            throw new ValidationException("Commodity Type " + commodityCode + " is not valid at row " + row);
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

    private void applyChargeableValidation(String transportMode, int row, Containers containersRow) throws RunnerException {
        if (containersRow.getChargeableUnit() != null && !containersRow.getChargeableUnit().isEmpty() &&
                transportMode != null && transportMode.equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR) &&
                containersRow.getChargeable() != null) {
            if (!containersRow.getChargeableUnit().equals(Constants.WEIGHT_UNIT_KG)) {
                throw new ValidationException("Chargeable unit not in KG at row: " + row);
            }
            var actualChargeable = containersRow.getChargeable();
            actualChargeable = CommonUtils.roundBigDecimal(actualChargeable, 2, RoundingMode.HALF_UP);
            BigDecimal calculatedChargeable = null;

            var vwob = getVolumeWeightChargeable(containersRow);
            if (vwob.getChargeable() != null) {
                calculatedChargeable = vwob.getChargeable();
                calculatedChargeable = CommonUtils.roundBigDecimal(calculatedChargeable, 2, RoundingMode.HALF_UP);
                if (!Objects.equals(calculatedChargeable, actualChargeable)) {
                    throw new ValidationException("Chargeable is invalid at row: " + row);
                }
            }
        }
    }

    private VolumeWeightChargeable getVolumeWeightChargeable(Containers containersRow) throws RunnerException {
        return consolidationService.calculateVolumeWeight(Constants.TRANSPORT_MODE_AIR,
                containersRow.getGrossWeightUnit() == null ? Constants.WEIGHT_UNIT_KG : containersRow.getGrossWeightUnit(),
                containersRow.getGrossVolumeUnit() == null ? Constants.VOLUME_UNIT_M3 : containersRow.getGrossVolumeUnit(),
                containersRow.getGrossWeight() == null ? BigDecimal.ZERO : containersRow.getGrossWeight(),
                containersRow.getGrossVolume() == null ? BigDecimal.ZERO : containersRow.getGrossVolume());
    }

    private void checkCalculatedVolumeAndActualVolume(BulkUploadRequest request, int row, Containers containersRow) {
        if (request.getTransportMode() != null && !StringUtility.isEmpty(request.getTransportMode()) && request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)
                && containersRow.getGrossVolume() != null) {
            if (!containersRow.getGrossVolumeUnit().equals(Constants.VOLUME_UNIT_M3)) {
                throw new ValidationException("Gross Volume unit not in M3 at row: " + (row + 1));
            }
            BigDecimal actualVolume = containersRow.getGrossVolume();
            actualVolume = CommonUtils.roundBigDecimal(actualVolume, 2, RoundingMode.HALF_UP);
            BigDecimal calculatedVolume = getCalculatedVolume(containersRow.getPackageBreadth(), containersRow.getPackageLength(), containersRow.getPackageHeight());
            if(calculatedVolume != null) calculatedVolume = CommonUtils.roundBigDecimal(calculatedVolume, 2, RoundingMode.HALF_UP);
            if (calculatedVolume != null && calculatedVolume.compareTo(actualVolume) != 0) {
                throw new ValidationException("Gross Volume is invalid at row: " + (row + 1));
            }
        } else if (request.getTransportMode() != null && request.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && containersRow.getGrossVolume() != null
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

    private static void applyContainerCountValidation(String transportMode, int row, Containers containersRow) {
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
    public void uploadContainerEvents(BulkUploadRequest request) throws RunnerException, IOException {
        if (request == null || request.getConsolidationId() == null) {
            throw new ValidationException("Please save the consolidation and then try again.");
        }
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        List<Events> eventsList = newParser.parseExcelFile(request.getFile(), request, null, masterDataMap, Events.class, ContainerEventExcelModel.class, null, null, locCodeToLocationReferenceGuidMap);
        eventsList = eventsList.stream().map(c -> {
            c.setEntityId(request.getConsolidationId());
            c.setEntityType("CONSOLIDATION");
            return c;
        }).toList();
        eventDao.saveAll(eventsList);
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

    @Override
    public void downloadContainerEvents(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException, IOException, IllegalAccessException {
        List<ContainerEventExcelModel> eventsModelList = new ArrayList<>();
        if (request.getConsolidationId() != null) {

            ListCommonRequest req = constructListCommonRequest(Constants.CONSOLIDATION_ID, Long.valueOf(request.getConsolidationId()), "=");
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
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
        String timestamp = currentTime.format(formatter);
        String filenameWithTimestamp = "Containers_Events_" + timestamp + Constants.XLSX;

        try(XSSFWorkbook workbook = new XSSFWorkbook()) {
            XSSFSheet sheet = workbook.createSheet("Containers_Events");
            convertModelToExcelForContainersEvent(eventsModelList, sheet);

            response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
            response.setHeader(Constants.CONTENT_DISPOSITION, Constants.ATTACHMENT_FILENAME + filenameWithTimestamp);

            try (OutputStream outputStream = response.getOutputStream()) {
                workbook.write(outputStream);
            }
        }
    }
    private void convertModelToExcelForContainersEvent(List<ContainerEventExcelModel> modelList, XSSFSheet sheet) throws IllegalAccessException {

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
        processModelList(modelList, sheet, rowIndex, fieldsList, actualFieldIdx, format, estimatedFieldIdx);
    }

    private void processModelList(List<ContainerEventExcelModel> modelList, XSSFSheet sheet, int rowIndex, List<Field> fieldsList, int actualFieldIdx, String format, int estimatedFieldIdx) throws IllegalAccessException {
        for (ContainerEventExcelModel model : modelList) {
            Row row = sheet.createRow(rowIndex++);
            int cellIndex = 0;
            for (Field field : fieldsList) {
                field.setAccessible(true);
                Object value = field.get(model);
                if(actualFieldIdx != -1 && cellIndex == actualFieldIdx){
                    assert value instanceof LocalDateTime;
                    value = ReportHelper.generateFormattedDate((LocalDateTime) value, format);
                }
                if(estimatedFieldIdx != -1 && cellIndex == estimatedFieldIdx){
                    assert value instanceof LocalDateTime;
                    value = ReportHelper.generateFormattedDate((LocalDateTime) value, format);
                }
                Cell cell = row.createCell(cellIndex++);
                cell.setCellValue(value != null ? value.toString() : "");
            }
        }
    }


    @Transactional
    public ResponseEntity<IRunnerResponse> attachPacks(Long containerId, List<Long> packsId) {
        Containers containers = containerDao.findById(containerId).get();

        if (containers != null) {
            for (Long packid : packsId) {
                Optional<Packing> packing = packingDao.findById(packid);
                if (packing.isPresent() && packing.get() != null) {
                    containers.getPacksList().add(packing.get());
                }
            }
            Containers entity = containerDao.save(containers);
            afterSave(entity, false);
            return ResponseHelper.buildSuccessResponse(jsonHelper.convertValue(entity, ContainerResponse.class));
        }

        return null;
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        return null;
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
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
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
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
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
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
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
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
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
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
            ContainerResponse response = (ContainerResponse) convertEntityToDto(container.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private Containers changeAchievedUnit(Containers container) throws RunnerException{
        try {
            if(!isStringNullOrEmpty(container.getAchievedVolumeUnit()) && !isStringNullOrEmpty(container.getAllocatedVolumeUnit()) && !container.getAchievedVolumeUnit().equals(container.getAllocatedVolumeUnit())) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());
                container.setAchievedVolume(val);
            }
            if(!isStringNullOrEmpty(container.getAchievedWeightUnit()) && !isStringNullOrEmpty(container.getAllocatedWeightUnit()) && !container.getAchievedWeightUnit().equals(container.getAllocatedWeightUnit())) {
                BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                container.setAchievedWeight(val);
            }
            container.setAchievedVolumeUnit(container.getAllocatedVolumeUnit());
            container.setAchievedWeightUnit(container.getAllocatedWeightUnit());
            container = calculateUtilization(container);
            return container;
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public Containers calculateUtilization(Containers container) {
        if(container == null)
            return null;
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
                container.setWeightUtilization( String.valueOf(CommonUtils.calculatePercentage(container.getAchievedWeight(), container.getAllocatedWeight(), 4, RoundingMode.HALF_UP)) );
        }
        else
            container.setWeightUtilization("0");
        if(container.getAllocatedVolume() != null) {
            if(container.getAchievedVolume().compareTo(container.getAllocatedVolume()) > 0)
                container.setVolumeUtilization("100");
            else if (Objects.equals(container.getAllocatedVolume(), BigDecimal.ZERO))
                container.setVolumeUtilization("0");
            else
                container.setVolumeUtilization( String.valueOf(CommonUtils.calculatePercentage(container.getAchievedVolume(), container.getAllocatedVolume(), 4, RoundingMode.HALF_UP)) );
        }
        else
            container.setVolumeUtilization("0");
        return container;
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateAchievedAllocatedForSameUnit(CommonRequestModel commonRequestModel) {
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

    @Override
    public ResponseEntity<IRunnerResponse> calculateAllocatedData(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CheckAllocatedDataChangesRequest request = (CheckAllocatedDataChangesRequest) commonRequestModel.getData();
            CheckAllocatedDataChangeResponse response = new CheckAllocatedDataChangeResponse();
            response.setVolumeAllowed(true);
            response.setWeightAllowed(true);
            if(request != null && !isStringNullOrEmpty(request.getContainerCode())) {
                CommonV1ListRequest listRequest = new CommonV1ListRequest();
                List<Object> criteria = Arrays.asList(
                        Arrays.asList(EntityTransferConstants.CODE),
                        "=",
                        request.getContainerCode()
                );
                listRequest.setCriteriaRequests(criteria);
                V1DataResponse v1DataResponse = v1Service.fetchContainerTypeData(listRequest);
                if(v1DataResponse != null && v1DataResponse.entities != null) {
                    List<EntityTransferContainerType> containerTypesList = jsonHelper.convertValueToList(v1DataResponse.entities, EntityTransferContainerType.class);
                    if(!containerTypesList.isEmpty()) {
                        EntityTransferContainerType containerType = containerTypesList.get(0);
                        setWeightVolumeInReseponse(request, containerType, response);
                    }
                }
            }
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private void setWeightVolumeInReseponse(CheckAllocatedDataChangesRequest request, EntityTransferContainerType containerType, CheckAllocatedDataChangeResponse response) throws RunnerException {
        if(request.getAllocatedVolume() != null && !isStringNullOrEmpty(request.getAllocatedVolumeUnit())
                && !isStringNullOrEmpty(containerType.getCubicCapacityUnit()) && containerType.getCubicCapacity() != null) {
            Double volume = (Double) convertUnit(Constants.VOLUME, request.getAllocatedVolume(), request.getAllocatedVolumeUnit(), containerType.getCubicCapacityUnit());
            if(volume > containerType.CubicCapacity)
                response.setVolumeAllowed(false);
        }
        if(request.getAllocatedWeight() != null && !isStringNullOrEmpty(request.getAllocatedWeightUnit())
                && !isStringNullOrEmpty(containerType.getMaxCargoGrossWeightUnit()) && containerType.getMaxCargoGrossWeight() != null) {
            Double weight = (Double) convertUnit(Constants.MASS, request.getAllocatedWeight(), request.getAllocatedWeightUnit(), containerType.getMaxCargoGrossWeightUnit());
            if(weight > containerType.MaxCargoGrossWeight)
                response.setWeightAllowed(false);
        }
    }

    public void changeContainerWtVolForSeaLCLDetach(Containers container, Packing packing) throws RunnerException {
        if(packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit()) && !isStringNullOrEmpty(container.getAchievedWeightUnit())) {
            BigDecimal val = new BigDecimal(convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), container.getAchievedWeightUnit()).toString());
            container.setAchievedWeight(container.getAchievedWeight().subtract(val));
        }
        if(packing.getVolume() != null && !isStringNullOrEmpty(packing.getVolumeUnit()) && !isStringNullOrEmpty(container.getAchievedVolumeUnit())) {
            BigDecimal val = new BigDecimal(convertUnit(Constants.VOLUME, packing.getVolume(), packing.getVolumeUnit(), container.getAchievedVolumeUnit()).toString());
            container.setAchievedVolume(container.getAchievedVolume().subtract(val));
        }
        calculateUtilization(container);
    }

    /**
     * Retrieves container responses based on the provided module GUID and module type.
     *
     * <p>This method determines whether the module type is SHIPMENT or CONSOLIDATION,
     * fetches the relevant container details, and returns them as a response.</p>
     *
     * <p>If an invalid module type is provided, the method logs an error and returns a failed response.
     * If the module GUID is not a valid UUID, it logs the issue and returns a failure response.</p>
     *
     * @param moduleGuid The unique identifier of the module, expected to be a valid UUID string.
     * @param moduleType The type of the module (e.g., "SHIPMENT", "CONSOLIDATION").
     * @return {@link ResponseEntity} containing a list of {@link ContainerResponse} if successful, or an error response if the request is invalid or data is not found.
     */
    @Override
    public ResponseEntity<IRunnerResponse> getByModuleGuidAndModuleType(String moduleGuid, String moduleType) {
        try {
            // Convert moduleGuid to UUID (throws IllegalArgumentException if invalid)
            UUID guid = UUID.fromString(moduleGuid);
            List<ContainerResponse> containerResponseList = switch (moduleType.toUpperCase()) {
                case Constants.SHIPMENT -> getContainerResponsesFromShipment(guid);
                case Constants.CONSOLIDATION -> getContainerResponsesFromConsolidation(guid);
                default -> throw new ValidationException("Invalid moduleType: " + moduleType);
            };

            return ResponseHelper.buildSuccessResponse(containerResponseList);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    /**
     * Fetches container responses for a given shipment based on its UUID.
     *
     * @param guid UUID of the shipment.
     * @return List of container responses.
     * @throws DataRetrievalFailureException if no shipment data is found.
     */
    private List<ContainerResponse> getContainerResponsesFromShipment(UUID guid) {
        ShipmentDetails shipmentDetails = shipmentDao.findByGuid(guid)
                .orElseThrow(() -> new DataRetrievalFailureException("Data not available for provided shipment request"));

        // Convert shipment's container list to List<ContainerResponse>
        return jsonHelper.convertValueToList(shipmentDetails.getContainersList().stream().toList(), ContainerResponse.class);
    }

    /**
     * Fetches container responses for a given consolidation based on its UUID.
     *
     * @param guid UUID of the consolidation.
     * @return List of container responses.
     * @throws DataRetrievalFailureException if no consolidation data is found.
     */
    private List<ContainerResponse> getContainerResponsesFromConsolidation(UUID guid) {
        ConsolidationDetails consolidationDetails = consolidationDetailsDao.findByGuid(guid)
                .orElseThrow(() -> new DataRetrievalFailureException("Data not available for provided consolidation request"));

        // Convert consolidation's container list to List<ContainerResponse>
        return jsonHelper.convertValueToList(consolidationDetails.getContainersList(), ContainerResponse.class);
    }

    public void changeContainerWtVolForSeaFCLDetach(Containers container) {
        container.setAchievedWeight(BigDecimal.ZERO);
        container.setAchievedVolume(BigDecimal.ZERO);
        container.setWeightUtilization("0");
        container.setVolumeUtilization("0");
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateAchievedQuantityOnPackDetach(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ContainerPackADInShipmentRequest request = (ContainerPackADInShipmentRequest) commonRequestModel.getData();

            Optional<Containers> containersOptional = containerDao.findById(request.getContainerId());
            if(containersOptional.isPresent()) {
                Containers container = containersOptional.get();
                changeAchievedUnit(container);
                ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).get();
                if(request.getPacksId() != null && !request.getPacksId().isEmpty()) {
                    ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", request.getShipmentId(), "=");
                    Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                    Page<Packing> allPackings = packingDao.findAll(pair.getLeft(), pair.getRight());
                    if(allPackings != null && !allPackings.isEmpty())
                    {
                        return processAllPackings(allPackings, request, shipmentDetails, container);
                    }
                }
                else {
                    if(shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        changeContainerWtVolForSeaFCLDetach(container);
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

    private ResponseEntity<IRunnerResponse> processAllPackings(Page<Packing> allPackings, ContainerPackADInShipmentRequest request, ShipmentDetails shipmentDetails, Containers container) throws RunnerException {
        List<Packing> packingList = allPackings.stream().toList();
        List<Packing> detachPacks = new ArrayList<>();
        boolean removeAllPacks = true;
        for(Packing packing: packingList) {
            if(request.getPacksId().contains(packing.getId())) {
                detachPacks.add(packing);
                if(!shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                    changeContainerWtVolForSeaLCLDetach(container, packing);
                }
            }
            else if(packing.getContainerId() != null && packing.getContainerId().equals(request.getContainerId()))
                removeAllPacks = false;
        }
        if(removeAllPacks && shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
            changeContainerWtVolForSeaFCLDetach(container);
        }
        return detachContainer(detachPacks, container, request.getShipmentId(), removeAllPacks);
    }

    public ResponseEntity<IRunnerResponse> detachContainer(List<Packing> packingList, Containers container, Long shipmentId, boolean removeAllPacks) {
        String responseMsg;
        try {
            Containers containers = containerDao.save(container);
            if(removeAllPacks)
                shipmentsContainersMappingDao.detachShipments(container.getId(), List.of(shipmentId), false);
            else {
                containerSync(containers);
            }
            if(packingList != null && !packingList.isEmpty()) {
                for (Packing packing: packingList) {
                    packing.setContainerId(null);
                }
                packingDao.saveAll(packingList);
                packingADSync(packingList);
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

    private void packingADSync(List<Packing> packingList) {
        try {
            packingsADSync.sync(packingList, UUID.randomUUID().toString());
        }
        catch (Exception e) {
            log.error("Error syncing packings");
        }
    }

    private void containerSync(Containers containers) {
        try {
            log.info("Call sync containers from detachContainer with ids: " + containers.toString());
            containersSync.sync(List.of(containers.getId()), shipmentsContainersMappingDao.findAllByContainerIds(List.of(containers.getId())));
        }
        catch (Exception e) {
            log.error("Error syncing containers");
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> getContainersForSelection(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        boolean lclAndSeaOrRoadFlag = shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled();
        boolean isConsolidatorFlag = shipmentSettingsDetails.getIsConsolidator() != null && shipmentSettingsDetails.getIsConsolidator();
        List<Containers> containersList = new ArrayList<>();
        try {
            ContainerAssignListRequest containerAssignRequest = (ContainerAssignListRequest) commonRequestModel.getData();
            Long shipmentId = containerAssignRequest.getShipmentId();
            Long consolidationId = containerAssignRequest.getConsolidationId();
            if (updateLclAndSeaOrRoadFlagFalse(lclAndSeaOrRoadFlag, containerAssignRequest)) {
                    lclAndSeaOrRoadFlag = false;
            }

            ListCommonRequest listCommonRequest = constructListCommonRequest(Constants.CONSOLIDATION_ID, consolidationId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = containerDao.findAll(pair.getLeft(), pair.getRight());
            List<Containers> conts = new ArrayList<>();
            if(lclAndSeaOrRoadFlag) {
                processContainers(containers, shipmentId, containersList, isConsolidatorFlag, conts);
                processConts(conts, containersList);
            }
            else {
                for (Containers container : containers.getContent()) {
                    List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
                    if(shipmentsContainersMappings.isEmpty()) {
                        containersList.add(container);
                    }
                }
            }
            if(containerAssignRequest.getTake() != null)
                containers = new PageImpl<>(containersList.subList(0, Math.min(containerAssignRequest.getTake(), containersList.size())), PageRequest.of(0, containerAssignRequest.getTake()), containersList.size());
            else
                containers = new PageImpl<>(containersList);
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoListWithMasterData(containers.getContent()),
                    containers.getTotalPages(),
                    containers.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private boolean updateLclAndSeaOrRoadFlagFalse(boolean lclAndSeaOrRoadFlag, ContainerAssignListRequest containerAssignRequest) {
        return lclAndSeaOrRoadFlag && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) && !containerAssignRequest.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA);
    }

    private void processConts(List<Containers> conts, List<Containers> containersList) {
        if(conts.isEmpty()) {
            return;
        }

        for (Containers x : conts) {
            boolean flag = true;
            if(x.getShipmentsList() != null && !x.getShipmentsList().isEmpty()) {
                for(ShipmentDetails shipmentDetails : x.getShipmentsList()) {
                    if (shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)) {
                        flag = false;
                        break;
                    }
                }
            }
            if (flag)
                containersList.add(x);
        }
    }

    private void processContainers(Page<Containers> containers, Long shipmentId, List<Containers> containersList, boolean isConsolidatorFlag, List<Containers> conts) throws RunnerException {
        for (Containers container : containers.getContent()) {
            List<ShipmentsContainersMapping> shipmentsContainersMappings = shipmentsContainersMappingDao.findByContainerId(container.getId());
            if(!shipmentsContainersMappings.stream().map(ShipmentsContainersMapping::getShipmentId).toList().contains(shipmentId)) {

                if(container.getAllocatedWeight() != null && container.getAchievedWeight() != null && container.getAllocatedVolume() != null && container.getAchievedVolume() != null
                   && isNotEmpty(container.getAllocatedWeightUnit()) && isNotEmpty(container.getAllocatedVolumeUnit()) && isNotEmpty(container.getAchievedWeightUnit()) && isNotEmpty(container.getAchievedVolumeUnit())) {

                    BigDecimal achievedWeight = new BigDecimal(convertUnit(Constants.MASS, container.getAchievedWeight(), container.getAchievedWeightUnit(), container.getAllocatedWeightUnit()).toString());
                    BigDecimal achievedVolume = new BigDecimal(convertUnit(Constants.VOLUME, container.getAchievedVolume(), container.getAchievedVolumeUnit(), container.getAllocatedVolumeUnit()).toString());

                    if(achievedWeight.compareTo(container.getAllocatedWeight()) < 0 && achievedVolume.compareTo(container.getAllocatedVolume()) < 0) {
                        containersList.add(container);
                    }
                    else if(!isConsolidatorFlag) {
                        conts.add(container);
                    }
                }
                else
                    containersList.add(container);
            }
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> validateContainerNumber(String containerNumber) {
        String responseMsg;
        try {
            ContainerNumberCheckResponse response = new ContainerNumberCheckResponse();
            response.setLastDigit(-1);
            if (containerNumber.length() != 10 && containerNumber.length() != 11) {
                response.setSuccess(false);
                return ResponseHelper.buildSuccessResponse(response);
            }
            ResponseEntity<IRunnerResponse> validationResponse = validateContainerNumberFormat(containerNumber, response);
            if (validationResponse != null) return validationResponse;
            List<Integer> eqvNumValue = assignEquivalentNumberValue();
            int expandedVal = getExpandedVal(containerNumber, eqvNumValue);
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

    private ResponseEntity<IRunnerResponse> validateContainerNumberFormat(String containerNumber, ContainerNumberCheckResponse response) {
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
        if (containerNumber.length() == 11 && ((int) containerNumber.charAt(10) < 48 || (int) containerNumber.charAt(10) > 57)) {
            response.setSuccess(false);
            return ResponseHelper.buildSuccessResponse(response);
        }
        return null;
    }

    private int getExpandedVal(String containerNumber, List<Integer> eqvNumValue) {
        int expandedVal = 0;
        for (int i = 0; i < 4; i++) {
            expandedVal = expandedVal + (int) Math.pow(2, i) * eqvNumValue.get((int) containerNumber.charAt(i) - 65);
        }
        for (int i = 4; i < 10; i++) {
            expandedVal = expandedVal + (int) Math.pow(2, i) * ((int) containerNumber.charAt(i) - 48);
        }
        return expandedVal;
    }

    @Override
    public ResponseEntity<IRunnerResponse> getContainers(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for container list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Containers>, Pageable> tuple = fetchData(request, Containers.class);
            Page<Containers> containersPage = containerDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Containers list for get containers retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            List<String> includeColumns;
            if (CollectionUtils.isEmpty(request.getIncludeColumns())) {
                includeColumns = defaultIncludeColumns;
            } else {
                includeColumns = request.getIncludeColumns();
            }
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(containersPage.getContent(), includeColumns),
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
    public ResponseEntity<IRunnerResponse> checkForDelete(CommonRequestModel commonRequestModel) {
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

    public ContainerSummaryResponse calculateContainerSummary(List<Containers> containersList, String transportMode, String containerCategory) throws RunnerException {
        try {
            double totalWeight = 0;
            double packageCount = 0;
            double tareWeight = 0;
            double totalVolume = 0;
            double totalContainerCount = 0;
            double totalPacks = 0;
            int dgContainers = 0;
            String toWeightUnit = Constants.WEIGHT_UNIT_KG;
            String toVolumeUnit = Constants.VOLUME_UNIT_M3;
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            if(!isStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
                toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
            if(!isStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
                toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
            if(containersList != null) {
                for (Containers containers : containersList) {
                    dgContainers = getDgContainers(containers, dgContainers);
                    double wInDef = convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), toWeightUnit).doubleValue();
                    double tarDef = convertUnit(Constants.MASS, containers.getTareWeight(), containers.getTareWeightUnit(), toWeightUnit).doubleValue();
                    double volume = convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), toVolumeUnit).doubleValue();
                    totalWeight = totalWeight + wInDef;
                    tareWeight = tareWeight + tarDef;
                    packageCount = getTotalPacks(containers, packageCount);
                    totalVolume = totalVolume + volume;
                    totalContainerCount = getTotalContainerCount(containers, totalContainerCount);
                    totalPacks = getTotalPacks(containers, totalPacks);
                }
            }
            ContainerSummaryResponse response = new ContainerSummaryResponse();
            response.setTotalPackages(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalPacks), 0, v1TenantSettingsResponse));
            response.setTotalContainers(IReport.getDPWWeightVolumeFormat(BigDecimal.valueOf(totalContainerCount), 0, v1TenantSettingsResponse));
            response.setTotalWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), v1TenantSettingsResponse), toWeightUnit));
            response.setTotalTareWeight(String.format(Constants.STRING_FORMAT, IReport.convertToWeightNumberFormat(BigDecimal.valueOf(tareWeight), v1TenantSettingsResponse), toWeightUnit));
            if(!isStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_SEA) &&
                    !isStringNullOrEmpty(containerCategory) && containerCategory.equals(Constants.SHIPMENT_TYPE_LCL)) {
                double volInM3 = convertUnit(Constants.VOLUME, BigDecimal.valueOf(totalVolume), toVolumeUnit, Constants.VOLUME_UNIT_M3).doubleValue();
                double wtInKg = convertUnit(Constants.MASS, BigDecimal.valueOf(totalWeight), toWeightUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
                double chargeableWeight = Math.max(wtInKg/1000, volInM3);
                chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
                response.setChargeableWeight(IReport.convertToWeightNumberFormat(BigDecimal.valueOf(chargeableWeight), v1TenantSettingsResponse) + " " + Constants.VOLUME_UNIT_M3);
            }
            response.setTotalContainerVolume(String.format(Constants.STRING_FORMAT, IReport.convertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), v1TenantSettingsResponse), toVolumeUnit));
            if(response.getSummary() == null)
                response.setSummary("");
            setContainerSummary(containersList, response);
            response.setDgContainers(dgContainers);
            return response;
        }
        catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    private void setContainerSummary(List<Containers> containersList, ContainerSummaryResponse response) {
        try {
            response.setSummary(calculateContainerSummary(containersList));
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

                updateSummaryForCountNoPart(response, summary);
                summary = new StringBuilder(summary.substring(0, summary.length() - 2));
                summary.append("), ");
            }

            if (containerCountPart > 0) {
                summary.append(" ").append(inWords(containerCountPart)).append("( Part of ");

                updateSummaryForCountPart(response, summary);
                summary = new StringBuilder(summary.substring(0, summary.length() - 2));
                summary.append(")");
            } else {
                summary = new StringBuilder(summary.substring(0, summary.length() - 2));
            }
            return summary.toString();
        }
        return null;
    }

    private void updateSummaryForCountPart(List<Containers> response, StringBuilder summary) {
        for (int i = 0; i < response.size(); i++) {
            Containers container = response.get(i);
            if (container.getIsPart() !=null && container.getIsPart()) {
                Long containerCount = container.getContainerCount();
                int j;
                for (j = i + 1; j < response.size(); j++) {
                    Containers nextContainer = response.get(j);
                    if (nextContainer.getContainerCode().equals(container.getContainerCode())) {
                        containerCount = getContainerCountForCountPart(nextContainer, containerCount);
                    } else {
                        break;
                    }
                }
                summary.append(container.getContainerCode()).append(" * ").append(containerCount).append(", ");
                i = j - 1;
            }
        }
    }

    private Long getContainerCountForCountPart(Containers nextContainer, Long containerCount) {
        if (nextContainer.getIsPart() !=null && nextContainer.getIsPart()) {
            containerCount = containerCount + nextContainer.getContainerCount();
        }
        return containerCount;
    }

    private void updateSummaryForCountNoPart(List<Containers> response, StringBuilder summary) {
        for (int i = 0; i < response.size(); i++) {
            Containers container = response.get(i);
            if (container.getIsPart() == null || !container.getIsPart()) {
                Long containerCount = container.getContainerCount();
                int j;
                for (j = i + 1; j < response.size(); j++) {
                    Containers nextContainer = response.get(j);
                    if (nextContainer.getContainerCode().equals(container.getContainerCode())) {
                        containerCount = getContainerCountForCountNoPart(nextContainer, containerCount);
                    } else {
                        break;
                    }
                }
                summary.append(container.getContainerCode()).append(" * ").append(containerCount).append(", ");
                i = j - 1;
            }
        }
    }

    private Long getContainerCountForCountNoPart(Containers nextContainer, Long containerCount) {
        if (nextContainer.getIsPart() ==null || !nextContainer.getIsPart()) {
            containerCount = containerCount + nextContainer.getContainerCount();
        }
        return containerCount;
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
        if(containers != null && !containers.isEmpty()) {
            for (Containers container : containers) {
                afterSave(container, isCreate);
            }
        }
    }

    public ResponseEntity<IRunnerResponse> containerSync(List<Long> request) {
        return containersSync.sync(request, shipmentsContainersMappingDao.findAllByContainerIds(request));
    }

    /**
     * V1 -> V2 sync
     */
    
    @Override
    public ResponseEntity<IRunnerResponse> v1ContainerCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException {
        ContainerRequestV2 containerRequest = (ContainerRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return ResponseHelper.buildSuccessResponse();
            }
            List<Containers> existingCont = containerDao.findByGuid(containerRequest.getGuid());
            Containers containers = syncEntityConversionService.containerV1ToV2(containerRequest);
            List<Long> shipIds = null;
            boolean isCreate = true;
            if (existingCont != null && !existingCont.isEmpty()) {
                containers.setId(existingCont.get(0).getId());
                containers.setConsolidationId(existingCont.get(0).getConsolidationId());
            } else {
                if (containerRequest.getConsolidationGuid() != null) {
                    Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findByGuid(containerRequest.getConsolidationGuid());
                    if (consolidationDetails.isPresent()) {
                        containers.setConsolidationId(consolidationDetails.get().getId());
                    }
                }
                shipIds = getShipIds(containerRequest, shipIds);
            }
            containers = containerDao.save(containers);
            afterSave(containers, isCreate);
            if (shipIds != null) {
                shipmentsContainersMappingDao.assignShipments(containers.getId(), shipIds, true);
            }
            ContainerResponse response = jsonHelper.convertValue(containers, ContainerResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
    }

    private List<Long> getShipIds(ContainerRequestV2 containerRequest, List<Long> shipIds) {
        if (containerRequest.getShipmentGuids() != null && !containerRequest.getShipmentGuids().isEmpty()) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("guid", containerRequest.getShipmentGuids(), "IN");
            Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
            Page<ShipmentDetails> shipmentDetails = shipmentDao.findAll(pair.getLeft(), pair.getRight());
            if (shipmentDetails.get() != null && shipmentDetails.get().count() > 0) {
                shipIds = shipmentDetails.get().map(e -> e.getId()).toList();
            }
        }
        return shipIds;
    }

    /**
     * Create bulk containers from V1 in V2
     */
    @Override
    public ResponseEntity<IRunnerResponse> v1BulkContainerCreateAndUpdate(CommonRequestModel commonRequestModel) {
        BulkContainerRequestV2 bulkContainerRequest = (BulkContainerRequestV2) commonRequestModel.getData();
        try {
            List<ResponseEntity<?>> responses = new ArrayList<>();
            for (ContainerRequestV2 containerRequest : bulkContainerRequest.getBulkContainers())
                responses.add(this.v1ContainerCreateAndUpdate(CommonRequestModel.builder()
                        .data(containerRequest)
                        .build(), true));
            return ResponseHelper.buildSuccessResponse(responses);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new GenericException(e);
        }
    }

    @Override
    public void exportContainers(HttpServletResponse response, ExportContainerListRequest request) throws RunnerException, IOException, IllegalAccessException {
        Optional<ConsolidationDetails> consol = Optional.empty();
        List<IRunnerResponse> containersList = null;
        if (request.getConsolidationId() != null) {
            consol = consolidationDetailsDao.findById(Long.valueOf(request.getConsolidationId()));

            validateConsoleAndContainersSize(consol);

            List<Containers> containers = consol.get().getContainersList();
            if (containers == null || containers.isEmpty()) {
                throw new GenericException("No containers present for this consol");
            }
            containersList = convertEntityListToDtoList(containers);

        } else {
            throw new GenericException("Consolidation does not exist, pls save the consol first");
        }

        try(Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("ContainersList");
            makeHeadersInSheet(sheet);

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
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "ContainerList_" + timestamp + Constants.XLSX;

            response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
            response.setHeader(Constants.CONTENT_DISPOSITION, Constants.ATTACHMENT_FILENAME + filenameWithTimestamp);

            try (OutputStream outputStream = response.getOutputStream()) {
                workbook.write(outputStream);
            }
        }

    }

    private void validateConsoleAndContainersSize(Optional<ConsolidationDetails> consol) {
        if (consol.isEmpty())
            throw new GenericException("Consolidation does not exist, pls save the consol first");

        if (consol.get().getContainersList().isEmpty())
            throw new GenericException("No containers found attached to consoliation");
    }

    private void makeHeadersInSheet(Sheet sheet) {
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

    private List<IRunnerResponse> convertEntityListToDtoListWithMasterData(List<Containers> lst) {
        Map<String, Object> cacheMap = new HashMap<>();
        List<IRunnerResponse> responseList = new ArrayList<>();
        Set<String> commodityTypes = new HashSet<>();
        Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
        lst.forEach(containers -> {
            ContainerResponse containerResponse = (ContainerResponse) convertEntityToDto(containers);
            responseList.add(containerResponse);
            commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(containerResponse, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + containers.getId(), cacheMap));
        });
        Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
        masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);
        if (!Objects.isNull(responseList)) {
            for (IRunnerResponse containerResponse : responseList) {
                ContainerResponse containerResponse1 = (ContainerResponse) containerResponse;
                containerResponse1.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + containerResponse1.getId()), CacheConstants.COMMODITY, cacheMap));
            }
        }
        return responseList;
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Containers> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(containers -> responseList.add(convertEntityToDto(containers)));
        return responseList;
    }
    private List<IRunnerResponse> convertEntityListToDtoList(List<Containers> lst, List<String> includeColumns) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        long start = System.currentTimeMillis();
        lst.forEach(containers -> responseList.add((IRunnerResponse) commonUtils.setIncludedFieldsToResponse(containers, includeColumns.stream().collect(Collectors.toSet()), new ContainerResponse())));
        log.info("Total time take to set container response {} ms", (System.currentTimeMillis() - start));
        return responseList;
    }

    public void pushContainersToDependentServices(List<Containers> containersList, List<Containers> oldContainers) {
        log.info("Starting pushContainersToDependentServices with containersList size: {} and oldContainers size: {}",
                containersList != null ? containersList.size() : 0,
                oldContainers != null ? oldContainers.size() : 0);
        V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
        log.debug("Tenant settings retrieved: LogicAppIntegrationEnabled={}, TransportOrchestratorEnabled={}",
                v1TenantSettingsResponse.getLogicAppIntegrationEnabled(),
                v1TenantSettingsResponse.getTransportOrchestratorEnabled());
        if(canProcessContainers(containersList, v1TenantSettingsResponse)) {
            EventMessage eventMessage = new EventMessage();
            eventMessage.setMessageType(ContainerConstants.CONTAINER_UPDATE_MSG);
            List<ContainerPayloadDetails> payloadDetails = getContainerPayloadDetails(containersList);
            if(CommonUtils.listIsNullOrEmpty(payloadDetails))
                return;
            ContainerUpdateRequest updateRequest = new ContainerUpdateRequest();
            updateRequest.setContainers(payloadDetails);
            updateRequest.setTenantCode(UserContext.getUser().getCode());
            eventMessage.setContainerUpdateRequest(updateRequest);
            String jsonBody = jsonHelper.convertToJson(eventMessage);
            log.debug("JSON body created for event message: {}", jsonBody);
            if (Boolean.TRUE.equals(v1TenantSettingsResponse.getTransportOrchestratorEnabled())) {
                log.info("Producing message to Kafka for transport orchestrator.");
                producer.produceToKafka(jsonBody, transportOrchestratorQueue, UUID.randomUUID().toString());
            }
            sbUtils.sendMessagesToTopic(isbProperties, messageTopic, List.of(new ServiceBusMessage(jsonBody)));
            log.info("Container pushed to kafka dependent services with data {}", jsonBody);
        }
    }

    private List<ContainerPayloadDetails> getContainerPayloadDetails(List<Containers> containersList) {
        List<ContainerPayloadDetails> payloadDetails = new ArrayList<>();
        for (Containers containers : containersList) {
            if(!StringUtility.isEmpty(containers.getContainerNumber()) && containers.getShipmentsList()!=null  && !containers.getShipmentsList().isEmpty()) {
                Set<ShipmentDetails> shipmentDetailsList = containers.getShipmentsList();
                for(ShipmentDetails shipmentDetail: shipmentDetailsList) {
                    String platformBookingRef = shipmentDetail.getBookingReference();
                    log.info("Platform Booking reference obtained: {}", platformBookingRef);
                    log.info("Preparing platform payload for container ID: {} with container number: {}",
                            containers.getId(), containers.getContainerNumber());
                    payloadDetails.add(prepareQueuePayload(containers, platformBookingRef));
                }
            }
        }
        return payloadDetails;
    }

    private boolean canProcessContainers(List<Containers> containersList, V1TenantSettingsResponse v1TenantSettingsResponse) {
        return containersList != null && !containersList.isEmpty() && (Boolean.TRUE.equals(v1TenantSettingsResponse.getLogicAppIntegrationEnabled())
                || Boolean.TRUE.equals(v1TenantSettingsResponse.getTransportOrchestratorEnabled()));
    }

    private ContainerPayloadDetails prepareQueuePayload(Containers containers, String bookingRef) {
        ContainerPayloadDetails details = new ContainerPayloadDetails();
        ContainerBoomiUniversalJson containerBoomiUniversalJson = modelMapper.map(containers, ContainerBoomiUniversalJson.class);
        if(Boolean.TRUE.equals(containerBoomiUniversalJson.getHazardous())) {
            containerBoomiUniversalJson.setCargoType(ContainerConstants.HAZ);
            containerBoomiUniversalJson.setHazardousGoodType(containers.getDgClass());
        }
        details.setBookingRef(bookingRef);
        details.setContainer(containerBoomiUniversalJson);
        return details;
    }

    private String getRefNum(Containers containers) {
        if(containers.getConsolidationId() != null) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDetailsDao.findById(containers.getConsolidationId());
            if(consolidationDetails.isPresent()) {
                return consolidationDetails.get().getReferenceNumber();
            }
        }
        return null;
    }

    @PostConstruct
    private void setDefaultIncludeColumns() {
        defaultIncludeColumns = FieldUtils.getNonRelationshipFields(Containers.class);
        defaultIncludeColumns.addAll(List.of("id","guid","tenantId"));
    }
}
