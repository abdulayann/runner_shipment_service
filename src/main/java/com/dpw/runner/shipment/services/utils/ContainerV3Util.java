package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentVersionContext;
import com.dpw.runner.shipment.services.commons.constants.*;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.mapper.ContainersMapper;
import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModelV3;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.MultiValidationException;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.service.impl.ContainerV3FacadeService;
import com.dpw.runner.shipment.services.service.interfaces.IApplicationConfigService;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotNull;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.HS_CODE_BATCH_PROCESS_LIMIT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.commons.constants.ContainerConstants.*;
import static com.dpw.runner.shipment.services.commons.constants.PackingConstants.PKG;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Component
public class ContainerV3Util {

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private MasterDataUtils masterDataUtils;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private MasterDataKeyUtils masterDataKeyUtils;

    @Autowired
    private CSVParsingUtil<Containers> parser;

    @Autowired
    private CSVParsingUtilV3<Containers> parserV3;

    @Autowired
    private ContainerV3Util containerUtilV3;

    @Autowired
    private IMDMServiceAdapter mdmServiceAdapter;

    @Autowired
    private ContainerV3FacadeService containerV3FacadeService;

    @Qualifier("asyncHsCodeValidationExecutor")
    @Autowired
    private ThreadPoolTaskExecutor hsCodeValidationExecutor;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private IApplicationConfigService applicationConfigService;
    @Autowired
    private IPackingDao packingDao;
    @Autowired
    private ShipmentsV3Util shipmentsV3Util;


    private final List<String> columnsSequenceForExcelDownload = List.of(
            "guid", "isOwnContainer", "isShipperOwned", "ownType", "isEmpty", "isReefer", "containerCode",
            "hblDeliveryMode", "containerNumber", "containerCount", "descriptionOfGoods", "handlingInfo",
            "hazardous", "dgClass", "hazardousUn", Constants.PACKS, Constants.PACKS_TYPE, "marksNums", "minTemp", "minTempUnit",
            "netWeight", "netWeightUnit", Constants.GROSS_WEIGHT, Constants.GROSS_WEIGHT_UNIT, "tareWeight", "tareWeightUnit", Constants.GROSS_VOLUME,
            Constants.GROSS_VOLUME_UNIT, "measurement", "measurementUnit", "commodityCode", "hsCode", "customsReleaseCode",
            "containerStuffingLocation", "pacrNumber", "containerComments", "sealNumber", "carrierSealNumber",
            "shipperSealNumber", "terminalOperatorSealNumber", "veterinarySealNumber", "customsSealNumber"
    );

    private final List<String> columnsSequenceForExcelDownloadForAir = List.of(
            "guid", "hblDeliveryMode", "descriptionOfGoods", "hazardous", "hazardousUn", Constants.PACKS, Constants.PACKS_TYPE,
            "marksNums", "serialNumber", "innerPackageNumber", "innerPackageType", "packageLength", "packageBreadth",
            "packageHeight", "innerPackageMeasurementUnit", "isTemperatureMaintained", "minTemp", "minTempUnit",
            "netWeight", "netWeightUnit", Constants.GROSS_WEIGHT, Constants.GROSS_WEIGHT_UNIT, "tareWeight", "tareWeightUnit", "chargeable",
            "chargeableUnit", Constants.GROSS_VOLUME, Constants.GROSS_VOLUME_UNIT, "commodityCode", "hsCode", "customsReleaseCode", "pacrNumber",
            "containerComments"
    );

    public void downloadContainers(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) {
        log.info("Initiating container download for request: {}", request);

        List<ContainersExcelModel> model;

        // STEP 1: Fetch container data
        try {
            model = fetchContainerExcelModel(request);
        } catch (RunnerException e) {
            log.error("Business error during container download: {}", e.getMessage(), e);
            sendJsonErrorResponse(response, e.getMessage());
            return;
        } catch (Exception e) {
            log.error("Unexpected error during data fetching: {}", e.getMessage(), e);
            sendJsonErrorResponse(response, "Failed to fetch container data. Please try again.");
            return;
        }

        // STEP 2: Generate Excel and write to response
        try {
            writeExcelToResponse(response, model, request);
            log.info("Excel file successfully written to response.");
        } catch (Exception e) {
            log.error("Failed to write Excel to response: {}", e.getMessage(), e);
            // Don't write to response here; it might be already committed.
        }
    }


    private List<ContainersExcelModel> fetchContainerExcelModel(BulkDownloadRequest request) throws RunnerException {
        List<Containers> result = new ArrayList<>();

        if (request.getShipmentId() != null) {
            ShipmentDetails shipment = shipmentDao.findById(Long.valueOf(request.getShipmentId()))
                    .orElseThrow(() -> new RunnerException("Shipment not found"));

            request.setTransportMode(shipment.getTransportMode());
            request.setExport(Constants.DIRECTION_EXP.equalsIgnoreCase(shipment.getDirection()));

            List<ShipmentsContainersMapping> mappings =
                    shipmentsContainersMappingDao.findByShipmentId(Long.valueOf(request.getShipmentId()));
            List<Long> containerIds = mappings.stream().map(ShipmentsContainersMapping::getContainerId).toList();

            ListCommonRequest req = constructListCommonRequest("id", containerIds, "IN");
            Page<Containers> containers = containerDao.findAll(fetchData(req, Containers.class).getLeft(),
                    fetchData(req, Containers.class).getRight());

            result.addAll(containers.getContent());
        }

        if (request.getConsolidationId() != null) {
            ConsolidationDetails consolidation = consolidationDetailsDao.findById(Long.valueOf(request.getConsolidationId()))
                    .orElseThrow(() -> new RunnerException("Consolidation not found"));

            request.setTransportMode(consolidation.getTransportMode());
            request.setExport(Constants.DIRECTION_EXP.equalsIgnoreCase(consolidation.getShipmentType()));

            ListCommonRequest req = constructListCommonRequest(Constants.CONSOLIDATION_ID, Long.valueOf(request.getConsolidationId()), "=");
            Page<Containers> containers = containerDao.findAll(fetchData(req, Containers.class).getLeft(),
                    fetchData(req, Containers.class).getRight());

            List<Containers> containersList = containers.getContent();

            if (result.isEmpty()) {
                result.addAll(containersList);
            } else {
                result = result.stream().filter(containersList::contains).toList();
            }
        }
        return getContainersExcelModels(result);
    }

    @NotNull
    public List<ContainersExcelModel> getContainersExcelModels(List<Containers> result) {
        Map<String, Set<String>> containerToShipmentMap = collectAllShipmentNumber(result);
        List<ContainersExcelModel> containersExcelModels = commonUtils.convertToList(result, ContainersExcelModel.class);
        containersExcelModels.forEach(p -> {
            if (containerToShipmentMap.containsKey(p.getGuid())) {
                p.setShipmentNumbers(String.join(", ", containerToShipmentMap.get(p.getGuid())));
            }
        });
        return containersExcelModels;
    }

    public Map<String, Set<String>> collectAllShipmentNumber(List<Containers> result) {
        List<ShipmentsContainersMapping> list = shipmentsContainersMappingDao.findByContainerIdIn(result.stream().map(Containers::getId).toList());
        Map<Long, Set<Long>> containerToShipmentsMap = list.stream().collect(Collectors.groupingBy(ShipmentsContainersMapping::getContainerId, Collectors.mapping(ShipmentsContainersMapping::getShipmentId, Collectors.toSet())));
        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(list.stream().map(ShipmentsContainersMapping::getShipmentId).collect(Collectors.toSet()));
        Map<Long, String> idToShipmentIdMap = shipmentDetails.stream().collect(Collectors.toMap(ShipmentDetails::getId, ShipmentDetails::getShipmentId));
        Map<Long, Set<String>> containerToShipmentCodesMap = containerToShipmentsMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().stream().map(idToShipmentIdMap::get).collect(Collectors.toSet())));
        Map<Long, String> idToGuid = result.stream().filter(p -> p.getGuid() != null).collect(Collectors.toMap(Containers::getId, p -> p.getGuid().toString()));
        return containerToShipmentCodesMap.entrySet().stream().filter(entry -> idToGuid.containsKey(entry.getKey())).collect(Collectors.toMap(entry -> idToGuid.get(entry.getKey()), Map.Entry::getValue));
    }


    private void writeExcelToResponse(HttpServletResponse response, List<ContainersExcelModel> model, BulkDownloadRequest request) throws IOException, IllegalAccessException {
        String timestamp = LocalDateTime.now()
                .format(DateTimeFormatter.ofPattern(Constants.YYYY_MM_DD_HH_MM_SS_FORMAT));
        String filename = "Containers_" + timestamp + Constants.XLSX;

        response.reset();
        response.setContentType(Constants.CONTENT_TYPE_FOR_EXCEL);
        response.setHeader(Constants.CONTENT_DISPOSITION, Constants.ATTACHMENT_FILENAME + filename);

        try (XSSFWorkbook workbook = new XSSFWorkbook();
                OutputStream outputStream = response.getOutputStream()) {

            XSSFSheet sheet = workbook.createSheet("Containers");
            convertModelToExcel(model, sheet, request);
            workbook.write(outputStream);
            outputStream.flush();
        }
    }

    private void sendJsonErrorResponse(HttpServletResponse response, String message) {
        try {
            if (!response.isCommitted()) {
                response.reset();
                response.setContentType("application/json");
                response.setCharacterEncoding("UTF-8");
                response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);

                ResponseEntity<IRunnerResponse> entity = ResponseHelper.buildFailedResponse(message, HttpStatus.INTERNAL_SERVER_ERROR);
                String json = objectMapper.writeValueAsString(entity.getBody());

                PrintWriter writer = response.getWriter();
                writer.write(json);
                writer.flush();
            } else {
                log.warn("Response already committed; cannot write JSON error.");
            }
        } catch (IOException e) {
            log.error("Failed to write JSON error to response", e);
        }
    }

    private void convertModelToExcel(List<ContainersExcelModel> modelList, XSSFSheet sheet, BulkDownloadRequest request) throws IllegalAccessException {
        Row headerRow = sheet.createRow(0);

        Map<String, Field> fieldNameMap = getRequiredFieldsMap();
        handleUnlocationsIfNeeded(modelList, request, fieldNameMap);

        List<Field> fieldsList = getReorderedFieldsList(request, fieldNameMap);

        createHeaderRowCells(headerRow, fieldsList);
        populateDataRows(modelList, sheet, fieldsList);
    }

    private Map<String, Field> getRequiredFieldsMap() {
        Field[] fields = ContainersExcelModel.class.getDeclaredFields();
        return Arrays.stream(fields)
                .filter(f -> f.isAnnotationPresent(ExcelCell.class))
                .filter(f -> f.getAnnotation(ExcelCell.class).requiredInV3())
                .collect(Collectors.toMap(Field::getName, f -> f));
    }

    private void handleUnlocationsIfNeeded(List<ContainersExcelModel> modelList, BulkDownloadRequest request, Map<String, Field> fieldNameMap) {
        if (!Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)
                && fieldNameMap.containsKey("containerStuffingLocation")) {
            Set<String> unlocationsRefGuids = new HashSet<>();
            processUnlocationsRefGuid(modelList, unlocationsRefGuids);
        }
    }

    private List<Field> getReorderedFieldsList(BulkDownloadRequest request, Map<String, Field> fieldNameMap) {
        if (isSeaOrRoadMode(request)) {
            return reorderFields(fieldNameMap, columnsSequenceForExcelDownload);
        } else if (Objects.equals(request.getTransportMode(), Constants.TRANSPORT_MODE_AIR)) {
            return reorderFields(fieldNameMap, columnsSequenceForExcelDownloadForAir);
        }
        return Collections.emptyList();
    }

    private boolean isSeaOrRoadMode(BulkDownloadRequest request) {
        String mode = request.getTransportMode();
        return Objects.equals(mode, Constants.TRANSPORT_MODE_SEA)
                || Objects.equals(mode, Constants.TRANSPORT_MODE_ROA)
                || Objects.equals(mode, Constants.TRANSPORT_MODE_RF)
                || Objects.equals(mode, Constants.TRANSPORT_MODE_RAI);
    }

    private void createHeaderRowCells(Row headerRow, List<Field> fieldsList) {
        int i = 0;
        for (Field field : fieldsList) {
            Cell cell = headerRow.createCell(i++);
            ExcelCell annotation = field.getAnnotation(ExcelCell.class);

            String displayName;
            if (ShipmentVersionContext.isV3()) {
                displayName = !annotation.displayNameOverride().isEmpty() ? annotation.displayNameOverride() : annotation.displayName();
            } else if (!annotation.displayName().isEmpty()) {
                displayName = annotation.displayName();
            } else {
                displayName = field.getName();
            }

            cell.setCellValue(displayName);
        }
    }


    private void populateDataRows(List<ContainersExcelModel> modelList, XSSFSheet sheet, List<Field> fieldsList) throws IllegalAccessException {
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

    public void addAllUnlocationInSingleCallList(List<ContainerBaseResponse> containers, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();

            containers.forEach(container -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(container, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + container.getId(), cacheMap)));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationInSingleCallListPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllCommodityTypesInSingleCall(List<ContainerBaseResponse> containers, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            containers.forEach(container -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(container, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + container.getId(), cacheMap)));

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCallListPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllContainerTypesInSingleCall(List<ContainerBaseResponse> containers, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> containerTypes = new HashSet<>();
            containers.forEach(container -> containerTypes.addAll(masterDataUtils.createInBulkContainerTypeRequest(container, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + container.getId(), cacheMap)));

            Map<String, EntityTransferContainerType> v1Data = masterDataUtils.fetchInBulkContainerTypes(containerTypes);
            masterDataUtils.pushToCache(v1Data, CacheConstants.CONTAINER_TYPE, containerTypes, new EntityTransferCommodityType(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.CONTAINER_TYPE, masterDataResponse, cacheMap);

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCallListPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllMasterDataInSingleCallList(List<ContainerBaseResponse> containers, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            containers.forEach(container -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(container, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + container.getId(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public BigDecimal getAddedWeight(BigDecimal initialWeight, String initialWeightUnit, BigDecimal addedWeight, String addedWeightUnit) throws RunnerException {
        if(Objects.isNull(addedWeight) || BigDecimal.ZERO.compareTo(addedWeight) == 0 || isStringNullOrEmpty(addedWeightUnit)) {
            return initialWeight;
        }
        if(Objects.isNull(initialWeight)) {
            initialWeight = BigDecimal.ZERO;
        }
        return initialWeight.add(new BigDecimal(convertUnit(Constants.MASS, addedWeight, addedWeightUnit, initialWeightUnit).toString()));
    }

    public BigDecimal getAddedVolume(BigDecimal initialVolume, String initialVolumeUnit, BigDecimal addedVolume, String addedVolumeUnit) throws RunnerException {
        if(Objects.isNull(addedVolume) || BigDecimal.ZERO.compareTo(addedVolume) == 0 || isStringNullOrEmpty(addedVolumeUnit)) {
            return initialVolume;
        }
        if(Objects.isNull(initialVolume)) {
            initialVolume = BigDecimal.ZERO;
        }
        return initialVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, addedVolume, addedVolumeUnit, initialVolumeUnit).toString()));
    }

    public void setContainerNetWeight(List<Containers> containers) throws RunnerException {
        for (Containers container : containers) {
            if (container.getTareWeight() != null && !Objects.equals(container.getTareWeight(), BigDecimal.ZERO)
                    && !isStringNullOrEmpty(container.getTareWeightUnit())) {
                container.setNetWeight(BigDecimal.ZERO);
                validateAndSetNetWeight(container);
                if (container.getGrossWeight() == null || BigDecimal.ZERO.equals(container.getGrossWeight()) || isStringNullOrEmpty(container.getGrossWeightUnit())) {
                    container.setNetWeight(container.getTareWeight());
                    container.setNetWeightUnit(container.getTareWeightUnit());
                    return;
                }
                container.setNetWeight(getAddedWeight(container.getNetWeight(), container.getNetWeightUnit(), container.getTareWeight(), container.getTareWeightUnit()));
                container.setNetWeight(getAddedWeight(container.getNetWeight(), container.getNetWeightUnit(), container.getGrossWeight(), container.getGrossWeightUnit()));
            } else {
                container.setNetWeight(container.getGrossWeight());
                container.setNetWeightUnit(container.getGrossWeightUnit());
            }
        }
    }

    private void validateAndSetNetWeight(Containers container) {
        if (isStringNullOrEmpty(container.getNetWeightUnit())) {
            container.setNetWeightUnit(
                    isStringNullOrEmpty(container.getGrossWeightUnit()) ?
                            commonUtils.getShipmentSettingFromContext().getWeightChargeableUnit() : container.getGrossWeightUnit());
        }
    }

    public void resetContainerDataForRecalculation(Containers container) {
        container.setNetWeight(BigDecimal.ZERO);
        container.setGrossVolume(BigDecimal.ZERO);
        container.setGrossWeight(BigDecimal.ZERO);
        container.setPacks(null);
        container.setPacksType(null);
    }

    public void addNoOfPackagesValueToContainer(Containers container, String packs, String packsType) {
        if(isStringNullOrEmpty(packs))
            return;
        addNoOfPackagesToContainer(container, Integer.parseInt(packs), packsType);
    }

    public void addNoOfPackagesToContainer(Containers container, Integer packs, String packsType) {
        if(isStringNullOrEmpty(packsType) || packs == null || packs == 0)
            return;
        if(isStringNullOrEmpty(container.getPacks()))
            container.setPacks(null);
        if(isStringNullOrEmpty(container.getPacksType()))
            container.setPacksType(packsType);
        else if(!Objects.equals(packsType, container.getPacksType()))
            container.setPacksType(PKG);
        container.setPacks(String.valueOf(Integer.parseInt(isStringNullOrEmpty(container.getPacks()) ? "0" : container.getPacks()) + packs));
    }

    public void setWtVolUnits(Containers containers) {
        if(isStringNullOrEmpty(containers.getGrossWeightUnit()))
            containers.setGrossWeightUnit(commonUtils.getDefaultWeightUnit());
        if(isStringNullOrEmpty(containers.getGrossVolumeUnit()))
            containers.setGrossVolumeUnit(commonUtils.getDefaultVolumeUnit());
    }

    public void setWtVolUnits(Containers containers, Packing packing) throws RunnerException {
        setContainerWeightUnit(containers, packing.getWeight(), packing.getWeightUnit());
        setContainerVolumeUnit(containers, packing.getVolume(), packing.getVolumeUnit());
        setContainerPacksUnit(containers, packing.getPacks(), packing.getPacksType());
    }

    public void setWtVolUnits(Containers containers, ShipmentDetails shipmentDetails) throws RunnerException {
        setContainerWeightUnit(containers, shipmentDetails.getWeight(), shipmentDetails.getWeightUnit());
        setContainerVolumeUnit(containers, shipmentDetails.getVolume(), shipmentDetails.getVolumeUnit());
        setContainerPacksUnit(containers, Objects.isNull(shipmentDetails.getNoOfPacks()) ? null : shipmentDetails.getNoOfPacks().toString(), shipmentDetails.getPacksUnit());
    }

    public void setContainerWeightUnit(Containers containers, BigDecimal sourceWeight, String sourceWeightUnit) throws RunnerException {
        if(!isStringNullOrEmpty(sourceWeightUnit) && sourceWeight != null && sourceWeight.compareTo(BigDecimal.ZERO) != 0) {
            if(containers.getGrossWeight() == null || BigDecimal.ZERO.compareTo(containers.getGrossWeight()) == 0) {
                containers.setGrossWeightUnit(sourceWeightUnit);
            } else {
                if(!sourceWeightUnit.equalsIgnoreCase(containers.getGrossWeightUnit())) {
                    String weightUnit = commonUtils.getDefaultWeightUnit();
                    containers.setGrossWeight(new BigDecimal(convertUnit(Constants.MASS, containers.getGrossWeight(), containers.getGrossWeightUnit(), weightUnit).toString()));
                    containers.setGrossWeightUnit(weightUnit);
                }
            }
        }
    }

    public void setContainerVolumeUnit(Containers containers, BigDecimal sourceVolume, String sourceVolumeUnit) throws RunnerException {
        if(!isStringNullOrEmpty(sourceVolumeUnit) && sourceVolume != null && sourceVolume.compareTo(BigDecimal.ZERO) != 0) {
            if(containers.getGrossVolume() == null || BigDecimal.ZERO.compareTo(containers.getGrossVolume()) == 0) {
                containers.setGrossVolumeUnit(sourceVolumeUnit);
            } else {
                if(!sourceVolumeUnit.equalsIgnoreCase(containers.getGrossVolumeUnit())) {
                    String volumeUnit = commonUtils.getDefaultVolumeUnit();
                    containers.setGrossVolume(new BigDecimal(convertUnit(Constants.VOLUME, containers.getGrossVolume(), containers.getGrossVolumeUnit(), volumeUnit).toString()));
                    containers.setGrossVolumeUnit(volumeUnit);
                }
            }
        }
    }

    public void setContainerPacksUnit(Containers containers, String sourcePacks, String sourcePacksType) {
        if(!isStringNullOrEmpty(sourcePacksType) && sourcePacks != null && Integer.parseInt(sourcePacks) != 0 &&
                (isStringNullOrEmpty(containers.getPacks()) || Integer.parseInt(containers.getPacks()) == 0))
            containers.setPacksType(sourcePacksType);
    }

    public String getContainerNumberOrType(Long containerId) {
        return getContainerNumberOrType(Objects.requireNonNull(containerDao.findById(containerId).orElse(null)));
    }

    public String getContainerNumberOrType(Containers container) {
        return isStringNullOrEmpty(container.getContainerNumber()) ? container.getContainerCode() : container.getContainerNumber();
    }

    @Transactional(rollbackFor = Exception.class)
    public void uploadContainers(BulkUploadRequest request, String module) throws IOException, RunnerException, MultiValidationException, InvocationTargetException, NoSuchMethodException {
        List<Containers> consolContainers = this.getContainerByModule(request, module);
        Map<UUID, Map<String, Object>> prevData = validationContainerUploadInShipment(consolContainers);
        Map<UUID, Containers> containerMap = consolContainers.stream().filter(Objects::nonNull).collect(Collectors.toMap(Containers::getGuid, Function.identity()));
        Map<UUID, Long> guidToIdMap = consolContainers.stream().collect(Collectors.toMap(Containers::getGuid, Containers::getId, (existing, replacement) -> existing));
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        List<String> excelHeaders = new ArrayList<>();
        List<String> errorList = new ArrayList<>();
        List<Containers> containersList = parserV3.parseExcelFile(request.getFile(), request, containerMap, masterDataMap, Containers.class, ContainersExcelModelV3.class, null, null, locCodeToLocationReferenceGuidMap, errorList, excelHeaders);
        parserV3.validateHeaders(excelHeaders, errorList);
        Map<UUID, Map<String, Object>> postData = validationContainerUploadInShipment(containersList);
        this.validateIfPacksOrVolume(prevData, postData, request, module, containersList, errorList, guidToIdMap);
        Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
        setIdAndTeuInContainers(request, containersList, guidToIdMap, codeTeuMap);
        validateHsCode(containersList, errorList);
        processErrorList(excelHeaders, errorList, containersList);
        List<ContainerV3Request> requests = ContainersMapper.INSTANCE.toContainerV3RequestList(containersList);
        containersList.forEach(p -> p.setContainerCount(1L));
        setShipmentOrConsoleId(request, module, requests);
        containerUtilV3.setContainerNetWeight(containersList);
        createOrUpdateContainers(requests, module);
    }

    private void processErrorList(List<String> excelHeaders, List<String> errorList, List<Containers> containersList) {
        if (CollectionUtils.isNotEmpty(errorList)) {
            Map<String, Integer> headerOrder = new HashMap<>();
            for (int i = 0; i < excelHeaders.size(); i++) {
                headerOrder.put(excelHeaders.get(i).toLowerCase(), i);
            }
            List<String> filteredErrors = errorList.stream().distinct().sorted(Comparator.comparingInt(this::extractRowNum).thenComparingInt(msg -> extractHeaderOrder(msg, headerOrder))).collect(Collectors.toList());
            Set<Integer> errorRows = filteredErrors.stream().map(this::extractRowNum).filter(rowNum -> rowNum > 1).collect(Collectors.toSet());
            int errorRowCount = errorRows.size();
            int totalContainers = containersList != null ? containersList.size() : 0;
            String summary = String.format("%d out of %d Container Details %s errors, Please rectify and upload.", errorRowCount, totalContainers, errorRowCount > 1 ? "contain" : "contains");
            throw new MultiValidationException(summary, filteredErrors);
        }
    }

    private int extractRowNum(String errorMsg) {
        try {
            int start = errorMsg.indexOf("Row#") + 4;
            int end = errorMsg.indexOf(":", start);
            if (start >= 0 && end > start) {
                return Integer.parseInt(errorMsg.substring(start, end).trim());
            }
        } catch (Exception e) {
            return Integer.MAX_VALUE;
        }
        return -1;
    }

    private int extractHeaderOrder(String msg, Map<String, Integer> headerOrder) {
        for (Map.Entry<String, Integer> entry : headerOrder.entrySet()) {
            if (msg.toLowerCase().contains(entry.getKey().toLowerCase())) {
                return entry.getValue();
            }
        }
        return Integer.MAX_VALUE; // unknown header → goes last
    }


    public List<Containers> getContainerByModule(BulkUploadRequest request, String module) {
        if (request == null) {
            throw new ValidationException("Please add the container and then try again.");
        }
        if (module.equalsIgnoreCase(CONSOLIDATION)) {
            if (request.getConsolidationId() == null || request.getConsolidationId() == 0) {
                throw new ValidationException("Please add the consolidation and then try again.");
            }
            return containerDao.findByConsolidationId(request.getConsolidationId());
        }
        if (module.equalsIgnoreCase(SHIPMENT)) {
            if (request.getShipmentId() == null || request.getShipmentId() == 0) {
                throw new ValidationException("Please add the shipment Id and then try again.");
            }
            return containerDao.findByShipmentId(request.getShipmentId());
        }
        throw new ValidationException(String.format("Module: %s; not found", module));
    }
    public void validateIfPacksOrVolume(Map<UUID, Map<String, Object>> from, Map<UUID, Map<String, Object>> to, BulkUploadRequest request, String module, List<Containers> containersList, List<String> errorList, Map<UUID, Long> guidToIdMap) {
        boolean isValidationRequired = false;
        if (module.equalsIgnoreCase(SHIPMENT)) {
            ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).orElseThrow(() -> new ValidationException("Shipment Id not exists"));
            if (CARGO_TYPE_FCL.equals(shipmentDetails.getShipmentType())) {
                isValidationRequired = !packingDao.findByContainerIdIn(containersList.stream().map(Containers::getId).toList()).isEmpty();
            }
        }
        final boolean validationRequired = isValidationRequired;
        Set<UUID> containerIdsWithPacking = getContainerIdsWithPacking(containersList, guidToIdMap);
        Set<UUID> containerIdsWithShipmentMapping = getContainerIdsWithShipmentMapping(containersList, guidToIdMap);
        Map<UUID, Integer> containerToRowMap = createContainerToRowMap(containersList);
        // Process each container that needs validation
        containersList.stream()
                .filter(container ->
                        containerIdsWithPacking.contains(container.getGuid()) ||
                        containerIdsWithShipmentMapping.contains(container.getGuid()) || validationRequired)
                .forEach(container -> {
                    UUID containerId = container.getGuid();
                    if (to.containsKey(containerId) && from.containsKey(containerId)) {
                        Map<String, Object> containersTo = to.get(containerId);
                        Map<String, Object> containersFrom = from.get(containerId);
                        int rowNumber = containerToRowMap.get(containerId);
                        validateBeforeAndAfterValues(containersTo, containersFrom, rowNumber, errorList);
                    }
                });
    }

    // Batch query for containers with packing
    private Set<UUID> getContainerIdsWithPacking(List<Containers> containersList, Map<UUID, Long> guidToIdMap) {

        // Single batch query instead of multiple individual queries
        List<Long> packingContainerIds = packingDao.findByContainerIdIn(containersList.stream().map(Containers::getId).toList())
                        .stream()
                        .map(Packing::getContainerId) // Adjust based on your Packing entity structure
                        .toList();
        return guidToIdMap.entrySet().stream()
                .filter(entry -> packingContainerIds.contains(entry.getValue())) // entry.getValue() is Long
                .map(Map.Entry::getKey) // entry.getKey() is UUID
                .collect(Collectors.toSet());
    }

    // Batch query for containers with shipment mapping
    private Set<UUID> getContainerIdsWithShipmentMapping(List<Containers> containersList, Map<UUID, Long> guidToIdMap) {
        List<Long> containerIds = containersList.stream()
                .map(Containers::getId)
                .toList();

        // Single batch query instead of individual queries per container
       List<Long> containerIdsFromShipContMapping = shipmentsContainersMappingDao.findByContainerIdIn(containerIds)
                .stream()
                .map(ShipmentsContainersMapping::getContainerId) // Adjust based on your mapping entity structure
                .toList();
        return guidToIdMap.entrySet().stream()
                .filter(entry -> containerIdsFromShipContMapping.contains(entry.getValue())) // entry.getValue() is Long
                .map(Map.Entry::getKey) // entry.getKey() is UUID
                .collect(Collectors.toSet());
    }

    // Create efficient lookup map for row numbers
    private Map<UUID, Integer> createContainerToRowMap(List<Containers> containersList) {
        Map<UUID, Integer> containerToRowMap = new HashMap<>();
        for (int i = 0; i < containersList.size(); i++) {
            containerToRowMap.put(containersList.get(i).getGuid(), i);
        }
        return containerToRowMap;
    }

    public static void validateBeforeAndAfterValues(Map<String, Object> containersTo,
                                                    Map<String, Object> containersFrom, int rowNum, List<String> errorList) {
            containersTo.keySet().forEach(fieldName -> {
            Object toValue = containersTo.get(fieldName);
            Object fromValue = containersFrom.get(fieldName);

            // Skip if both values are null
            if (toValue == null && fromValue == null) {
                return;
            }

            // Check if values have changed
            boolean hasChanged = isNullMismatch(toValue, fromValue) ||
                    isBigDecimalChangeInvalid(toValue, fromValue) ||
                    (!(toValue instanceof BigDecimal || fromValue instanceof BigDecimal) &&
                            !Objects.equals(toValue, fromValue));

            if (hasChanged) {
                switch (fieldName) {
                    case Constants.GROSS_VOLUME:
                        errorList.add(String.format(VOLUME_ERROR_LOG, rowNum + 2));
                        break;

                    case Constants.GROSS_VOLUME_UNIT:
                        errorList.add(String.format(VOLUME_ERROR_LOG, rowNum + 2));
                        break;

                    case Constants.CARGO_WEIGHT:
                        errorList.add(String.format(CARGO_WT_ERROR_LOG, rowNum + 2));
                        break;

                    case Constants.CARGO_WEIGHT_UNIT:
                        errorList.add(String.format(CARGO_WT_ERROR_LOG, rowNum + 2));
                        break;

                    case Constants.GROSS_WEIGHT:
                        errorList.add(String.format(GROSS_WT_ERROR_LOG, rowNum + 2));
                        break;

                    case Constants.GROSS_WEIGHT_UNIT:
                        errorList.add(String.format(GROSS_WT_ERROR_LOG, rowNum + 2));
                        break;

                    case Constants.PACKS:
                        errorList.add(String.format(PACKAGE_ERROR_LOG, rowNum + 2));
                        break;

                    case Constants.PACKS_TYPE:
                        errorList.add(String.format(PACKAGE_ERROR_LOG, rowNum + 2));
                        break;
                    default:
                        errorList.add(String.format("Unknown field: %s at row %d", fieldName, rowNum + 2));
                        break;

                }
            }
        });
    }

    private static boolean isNullMismatch(Object toValue, Object fromValue) {
        return (toValue == null && fromValue != null) || (toValue != null && fromValue == null);
    }

    private static boolean isBigDecimalChangeInvalid(Object toValue, Object fromValue) {
        if (toValue instanceof BigDecimal || fromValue instanceof BigDecimal) {
            if (isNullMismatch(toValue, fromValue)) {
                return true;
            }
            return ((BigDecimal) toValue).compareTo((BigDecimal) fromValue) > 0;
        }
        return false;
    }

    public Map<UUID, Map<String, Object>> validationContainerUploadInShipment(List<Containers> consolContainers) {
        Map<UUID, Map<String, Object>> map = new HashMap<>();
        for (Containers containers : consolContainers) {
            map.putIfAbsent(containers.getGuid(), new HashMap<>());
            map.get(containers.getGuid()).put(Constants.GROSS_VOLUME, containers.getGrossVolume());
            map.get(containers.getGuid()).put(Constants.GROSS_VOLUME_UNIT, containers.getGrossVolumeUnit());
            map.get(containers.getGuid()).put(Constants.CARGO_WEIGHT, containers.getGrossWeight());
            map.get(containers.getGuid()).put(CARGO_WEIGHT_UNIT, containers.getGrossWeightUnit());
            map.get(containers.getGuid()).put(GROSS_WEIGHT, containers.getNetWeight());
            map.get(containers.getGuid()).put(GROSS_WEIGHT_UNIT, containers.getNetWeightUnit());
            map.get(containers.getGuid()).put(Constants.PACKS, containers.getPacks());
            map.get(containers.getGuid()).put(Constants.PACKS_TYPE, containers.getPacksType());
        }
        return map;
    }
    public static void setShipmentOrConsoleId(BulkUploadRequest request, String module, List<ContainerV3Request> requests) {
        requests.forEach(p -> {
            if (module.equalsIgnoreCase(SHIPMENT)) {
                p.setShipmentId(request.getShipmentId());
            }
            if (module.equalsIgnoreCase(CONSOLIDATION)) {
                p.setConsolidationId(request.getConsolidationId());
            }
        });
    }

    private static void setIdAndTeuInContainers(BulkUploadRequest request, List<Containers> containersList, Map<UUID, Long> guidToIdMap, Map<String, BigDecimal> codeTeuMap) {
        containersList.forEach(container -> {
            if (container.getGuid() != null && guidToIdMap.containsKey(container.getGuid())) {
                container.setId(guidToIdMap.get(container.getGuid()));
            }
            if (container.getContainerCode() != null && codeTeuMap.containsKey(container.getContainerCode())) {
                container.setTeu(codeTeuMap.get(container.getContainerCode()));
            }
            container.setConsolidationId(request.getConsolidationId());
        });
    }

    public void createOrUpdateContainers(List<ContainerV3Request> requests, String module) throws RunnerException {
        for (int i = 0; i < requests.size(); i++) {
            try {
                containerV3FacadeService.createUpdateContainer(List.of(requests.get(i)), module);
            } catch (Exception e) {
                throw new RunnerException(String.format("Error processing row %d: %s", i + 1, e.getMessage()), e);
            }
        }
    }

    public void validateHsCode(List<Containers> containersList, List<String> errorList) {
        if (!containersList.isEmpty()) {
            List<Map<Boolean, Boolean>> validationHelperList = new ArrayList<>();
            Set<String> validHsCode = getValidHsCodes(syncCommodityAndHsCode(containersList, errorList, validationHelperList));
            for (int i = 0; i < containersList.size(); i++) {
                String hsCode = containersList.get(i).getHsCode();
                if (StringUtils.isNotBlank(hsCode) && !hsCode.contains(",") && !validHsCode.contains(hsCode)) {
                    validateHsCodeAndCommodityCode(errorList, i, hsCode, validationHelperList.get(i));
                }
            }
        }
    }

    private void validateHsCodeAndCommodityCode(List<String> errorList, int i, String hsCode, Map<Boolean, Boolean> validationHelperMap) {
        Map.Entry<Boolean, Boolean> entry = validationHelperMap.entrySet().iterator().next();
        if (Boolean.TRUE.equals(entry.getKey())){
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, i + 2, "HS Code", hsCode));
        } else if (Boolean.TRUE.equals(entry.getValue())) {
            errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, i + 2, "Commodity", hsCode));
        }
    }

    public static Set<String> syncCommodityAndHsCode(List<Containers> containersList, List<String> errorList, List<Map<Boolean, Boolean>> validationHelperList) {
        Set<String> hsCodeList = new HashSet<>();
        for (int i = 0; i < containersList.size(); i++) {
            Map<Boolean, Boolean> validationHelperMap = new HashMap<>();
            boolean isHsCode = false;
            boolean isCommodity = false;
            Containers container = containersList.get(i);
            String hsCode = container.getHsCode();
            String commodityCode = container.getCommodityCode();
            if (StringUtils.isBlank(commodityCode) && StringUtils.isNotBlank(hsCode)) {
                String firstHsCode = Arrays.stream(hsCode.split(","))
                        .findFirst()
                        .orElse(null);
                if (StringUtils.isBlank(firstHsCode)) {
                    errorList.add(String.format(ContainerConstants.GENERIC_INVALID_FIELD_MSG, i + 2, "HS Code", hsCode));
                } else {
                    isHsCode = true;
                    container.setCommodityCode(firstHsCode);
                }
            } else if (StringUtils.isBlank(hsCode) && StringUtils.isNotBlank(commodityCode)) {
                isCommodity = true;
                container.setHsCode(commodityCode);
            }
            validationHelperMap.put(isHsCode, isCommodity);
            if (StringUtils.isNotBlank(container.getHsCode()) && !container.getHsCode().contains(",")) {
                hsCodeList.add(container.getHsCode());
            }
            validationHelperList.add(validationHelperMap);
        }
        return hsCodeList;
    }

    public Integer getHsCodeBatchProcessLimit() {
        String configuredLimitValue = applicationConfigService.getValue(HS_CODE_BATCH_PROCESS_LIMIT);
        return StringUtility.isEmpty(configuredLimitValue) ? BATCH_HS_CODE_PROCESS_LIMIT : Integer.parseInt(configuredLimitValue);
    }

    public Set<String> getValidHsCodes(Set<String> hsCode) {
        if (hsCode.isEmpty()) {
            return new HashSet<>();
        }
        List<String> hsCodeList = new ArrayList<>(hsCode);
        List<List<String>> batches = new ArrayList<>();
        int batchSize = getHsCodeBatchProcessLimit();
        for (int i = 0; i < hsCodeList.size(); i += batchSize) {
            batches.add(hsCodeList.subList(i, Math.min(i + batchSize, hsCodeList.size())));
        }
        try {
            List<CompletableFuture<Set<String>>> futures = batches.stream()
                    .map(batch -> CompletableFuture.supplyAsync(() -> {
                        V1DataResponse response = parser.getCommodityDataResponse(new ArrayList<>(batch));
                        if (response != null && response.entities instanceof List<?>) {
                            List<CommodityResponse> commodityList = jsonHelper.convertValueToList(response.entities, CommodityResponse.class);
                            if (commodityList != null && !commodityList.isEmpty()) {
                                return commodityList.stream().filter(Objects::nonNull).map(CommodityResponse::getCode).filter(Objects::nonNull).collect(Collectors.toSet());
                            }
                        }
                        return Collections.<String>emptySet();
                    }, hsCodeValidationExecutor))
                    .toList();
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
            return futures.stream().map(CompletableFuture::join).flatMap(Set::stream).collect(Collectors.toSet());
        } catch (Exception e) {
            log.error("Error during HS code validation batch processing {}", e.getMessage());
            return new HashSet<>();
        }
    }

    public Map<String, BigDecimal> getCodeTeuMapping() throws RunnerException {
        DependentServiceResponse mdmResponse = mdmServiceAdapter.getContainerTypes();
        List<MdmContainerTypeResponse> containerTypes = jsonHelper.convertValueToList(mdmResponse.getData(), MdmContainerTypeResponse.class);
        return containerTypes.stream().collect(Collectors.toMap(MdmContainerTypeResponse::getCode, MdmContainerTypeResponse::getTeu));
    }

    public void createOrUpdateContainers(List<ContainerV3Request> requests) throws RunnerException {
        createOrUpdateContainers(requests, CONSOLIDATION);
    }

    public void updatedContainerResponseForLCLandLTL(List<ContainerBaseResponse> containerBaseResponses, ShipmentDetails shipmentDetails) throws RunnerException {
        if (shipmentDetails.getContainerAssignedToShipmentCargo() != null) {
            updateContainerFromShipmentCargo(containerBaseResponses, shipmentDetails);
        } else {
            updateContainersFromAssignedPackings(containerBaseResponses, shipmentDetails);
        }
    }

    private void updateContainerFromShipmentCargo(List<ContainerBaseResponse> containerBaseResponses, ShipmentDetails shipmentDetails) {
        Long containerId = shipmentDetails.getContainerAssignedToShipmentCargo();

        containerBaseResponses.stream()
                .filter(c -> Objects.equals(c.getId(), containerId))
                .findFirst()
                .ifPresent(containerBaseResponse -> {
                    containerBaseResponse.setGrossWeight(shipmentDetails.getWeight());
                    containerBaseResponse.setGrossWeightUnit(shipmentDetails.getWeightUnit());

                    containerBaseResponse.setNetWeight(shipmentDetails.getWeight());
                    containerBaseResponse.setNetWeightUnit(shipmentDetails.getWeightUnit());

                    containerBaseResponse.setGrossVolume(shipmentDetails.getVolume());
                    containerBaseResponse.setGrossVolumeUnit(shipmentDetails.getVolumeUnit());

                    if (shipmentDetails.getNoOfPacks() != null) {
                        containerBaseResponse.setPacks(String.valueOf(shipmentDetails.getNoOfPacks()));
                    }
                    containerBaseResponse.setPacksType(shipmentDetails.getPacksUnit());
                });
    }

    private void updateContainersFromAssignedPackings(List<ContainerBaseResponse> containerBaseResponses, ShipmentDetails shipmentDetails) throws RunnerException {
        List<Packing> packings = packingDao.findByShipmentId(shipmentDetails.getId());
        if (packings == null || packings.isEmpty()) {
            return;
        }

        Map<Long, ContainerBaseResponse> containerMap = containerBaseResponses.stream()
                .collect(Collectors.toMap(ContainerBaseResponse::getId, c -> c));

        Map<Long, List<Packing>> packingsByContainer = packings.stream()
                .filter(p -> p.getContainerId() != null)
                .collect(Collectors.groupingBy(Packing::getContainerId));

        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        String defaultWeightUnit = resolveDefaultUnit(shipmentSettingsDetails.getWeightChargeableUnit(), Constants.WEIGHT_UNIT_KG);
        String defaultVolumeUnit = resolveDefaultUnit(shipmentSettingsDetails.getVolumeChargeableUnit(), Constants.VOLUME_UNIT_M3);

        for (Map.Entry<Long, List<Packing>> entry : packingsByContainer.entrySet()) {
            Long containerId = entry.getKey();
            List<Packing> containerPackings = entry.getValue();
            ContainerBaseResponse container = containerMap.get(containerId);

            if (container != null) {
                updateContainerFromPackingList(container, containerPackings, defaultWeightUnit, defaultVolumeUnit);
            }
        }
    }

    private void updateContainerFromPackingList(ContainerBaseResponse container, List<Packing> containerPackings, String defaultWeightUnit, String defaultVolumeUnit) throws RunnerException {
        double totalWeight = 0;
        double totalVolume = 0;
        int totalPacks = 0;

        String targetWeightUnit = shipmentsV3Util.resolveUnit(containerPackings.stream().map(Packing::getWeightUnit).toList(), defaultWeightUnit);
        String targetVolumeUnit = shipmentsV3Util.resolveUnit(containerPackings.stream().map(Packing::getVolumeUnit).toList(), defaultVolumeUnit);
        String packType = shipmentsV3Util.resolveUnit(containerPackings.stream().map(Packing::getPacksType).toList(), PKG);

        for (Packing packing : containerPackings) {
            totalWeight += calculateWeight(packing, targetWeightUnit);
            totalVolume += calculateVolume(packing, targetVolumeUnit);
            totalPacks += parsePacks(packing);
        }

        container.setGrossWeight(BigDecimal.valueOf(totalWeight));
        container.setGrossWeightUnit(targetWeightUnit);

        container.setNetWeight(BigDecimal.valueOf(totalWeight));
        container.setNetWeightUnit(targetWeightUnit);

        container.setGrossVolume(BigDecimal.valueOf(totalVolume));
        container.setGrossVolumeUnit(targetVolumeUnit);

        container.setPacks(String.valueOf(totalPacks));
        container.setPacksType(packType);
    }

    private String resolveDefaultUnit(String unitFromSettings, String fallback) {
        return !isStringNullOrEmpty(unitFromSettings) ? unitFromSettings : fallback;
    }

    private double calculateWeight(Packing packing, String targetWeightUnit) throws RunnerException {
        if (packing.getWeight() == null) return 0;
        return convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), targetWeightUnit).doubleValue();
    }

    private double calculateVolume(Packing packing, String targetVolumeUnit) throws RunnerException {
        if (packing.getVolume() == null) return 0;
        return convertUnit(VOLUME, packing.getVolume(), packing.getVolumeUnit(), targetVolumeUnit).doubleValue();
    }

    private int parsePacks(Packing packing) {
        if (packing.getPacks() == null) return 0;
        return Integer.parseInt(packing.getPacks());
    }
}
