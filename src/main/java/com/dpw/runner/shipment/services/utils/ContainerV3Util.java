package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
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
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.dto.response.MdmContainerTypeResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
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
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
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
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.ApplicationConfigConstants.HS_CODE_BATCH_PROCESS_LIMIT;
import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
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
    private IMDMServiceAdapter mdmServiceAdapter;

    @Autowired
    private ContainerV3FacadeService containerV3FacadeService;

    @Qualifier("asyncHsCodeValidationExecutor")
    @Autowired
    private ThreadPoolTaskExecutor hsCodeValidationExecutor;

    private ObjectMapper objectMapper;

    @Autowired
    private IApplicationConfigService applicationConfigService;
    @Autowired
    private IPackingDao packingDao;


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

    public void downloadContainers(HttpServletResponse response, @ModelAttribute BulkDownloadRequest request) {
        log.info("Initiating container download for request: {}", request);

        List<ContainersExcelModel> model;

        // STEP 1: Fetch container data
        try {
            model = fetchContainerExcelModel(request);
            if (model.isEmpty()) {
                log.warn("No containers found for request: {}", request);
                sendJsonErrorResponse(response, "No containers found for given input.");
                return;
            }
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
        if(isStringNullOrEmpty(initialWeightUnit)) {
            initialWeightUnit = commonUtils.getShipmentSettingFromContext().getWeightChargeableUnit();
        }
        if(Objects.isNull(initialWeight)) {
            initialWeight = BigDecimal.ZERO;
        }
        if(Objects.isNull(addedWeight) || BigDecimal.ZERO.equals(addedWeight) || isStringNullOrEmpty(addedWeightUnit)) {
            return initialWeight;
        }
        return initialWeight.add(new BigDecimal(convertUnit(Constants.MASS, addedWeight, addedWeightUnit, initialWeightUnit).toString()));
    }

    public BigDecimal getAddedVolume(BigDecimal initialVolume, String initialVolumeUnit, BigDecimal addedVolume, String addedVolumeUnit) throws RunnerException {
        if(isStringNullOrEmpty(initialVolumeUnit)) {
            initialVolumeUnit = commonUtils.getShipmentSettingFromContext().getVolumeChargeableUnit();
        }
        if(Objects.isNull(initialVolume)) {
            initialVolume = BigDecimal.ZERO;
        }
        if(Objects.isNull(addedVolume) || BigDecimal.ZERO.equals(addedVolume) || isStringNullOrEmpty(addedVolumeUnit)) {
            return initialVolume;
        }
        return initialVolume.add(new BigDecimal(convertUnit(Constants.VOLUME, addedVolume, addedVolumeUnit, initialVolumeUnit).toString()));
    }

    public void setContainerNetWeight(Containers container) throws RunnerException {
        if(container.getTareWeight() != null && !Objects.equals(container.getTareWeight(), BigDecimal.ZERO)
                && !isStringNullOrEmpty(container.getTareWeightUnit())) {
            container.setNetWeight(BigDecimal.ZERO);
            if(isStringNullOrEmpty(container.getNetWeightUnit())) {
                container.setNetWeightUnit(
                        isStringNullOrEmpty(container.getGrossWeightUnit()) ?
                                commonUtils.getShipmentSettingFromContext().getWeightChargeableUnit() : container.getGrossWeightUnit());
            }
            if(container.getGrossWeight() == null || BigDecimal.ZERO.equals(container.getGrossWeight()) || isStringNullOrEmpty(container.getGrossWeightUnit())) {
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
    
    public String getContainerNumberOrType(Long containerId) {
        return getContainerNumberOrType(Objects.requireNonNull(containerDao.findById(containerId).orElse(null)));
    }
    
    public String getContainerNumberOrType(Containers container) {
        return isStringNullOrEmpty(container.getContainerNumber()) ? container.getContainerCode() : container.getContainerNumber();
    }

    @Transactional(rollbackFor = Exception.class)
    public void uploadContainers(BulkUploadRequest request, String module) throws IOException, RunnerException {
        List<Containers> consolContainers = this.getContainerByModule(request, module);
        Map<UUID, Map<String, Object>> prevData = validationContainerUploadInShipment(consolContainers);
        Map<UUID, Containers> containerMap = consolContainers.stream().filter(Objects::nonNull).collect(Collectors.toMap(Containers::getGuid, Function.identity()));
        Map<UUID, Long> guidToIdMap = consolContainers.stream().collect(Collectors.toMap(Containers::getGuid, Containers::getId, (existing, replacement) -> existing));
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        List<Containers> containersList = parser.parseExcelFile(request.getFile(), request, containerMap, masterDataMap, Containers.class, ContainersExcelModel.class, null, null, locCodeToLocationReferenceGuidMap);
        Map<UUID, Map<String, Object>> postData = validationContainerUploadInShipment(containersList);
        this.validateIfPacksOrVolume(prevData, postData, request, module, containersList);
        Map<String, BigDecimal> codeTeuMap = getCodeTeuMapping();
        setIdAndTeuInContainers(request, containersList, guidToIdMap, codeTeuMap);
        validateHsCode(containersList);
        List<ContainerV3Request> requests = ContainersMapper.INSTANCE.toContainerV3RequestList(containersList);
        setShipmentOrConsoleId(request, module, requests);
        createOrUpdateContainers(requests, module);
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
    @Transactional(rollbackFor = Exception.class)
    public void uploadContainers(BulkUploadRequest request) throws IOException, RunnerException {
        uploadContainers(request, CONSOLIDATION);
    }
    public void validateIfPacksOrVolume(Map<UUID, Map<String, Object>> from, Map<UUID, Map<String, Object>> to, BulkUploadRequest request, String module, List<Containers> containersList) {
        boolean isValidationRequired = module.equalsIgnoreCase(CONSOLIDATION);
        if (module.equalsIgnoreCase(SHIPMENT)) {
            ShipmentDetails shipmentDetails = shipmentDao.findById(request.getShipmentId()).orElseThrow(() -> new ValidationException("Shipment Id not exists"));
            if (CARGO_TYPE_FCL.equals(shipmentDetails.getShipmentType())) {
                isValidationRequired = !packingDao.findByContainerIdIn(containersList.stream().map(Containers::getId).toList()).isEmpty();
            }
        }
        if (isValidationRequired) {
            for (UUID containerId : to.keySet()) {
                if (from.containsKey(containerId)) {
                    Map<String, Object> containersTo = to.get(containerId);
                    Map<String, Object> containersFrom = from.get(containerId);
                    validateBeforeAndAfterValues(containerId, containersTo, containersFrom);
                }
            }
        }
    }
    public static void validateBeforeAndAfterValues(UUID containerId, Map<String, Object> containersTo, Map<String, Object> containersFrom) {
        for (String key : containersTo.keySet()) {
            if (containersTo.get(key) instanceof BigDecimal) {
                if (((BigDecimal) containersTo.get(key)).compareTo((BigDecimal) containersFrom.get(key)) > 0) {
                    throw new ValidationException(String.format("%s, Cannot be Changes as Package, Weight and Volume Details Update not allowed in Upload. for container GUID: %s", key, containerId));
                }
            } else if (!Objects.equals(containersTo.get(key), containersFrom.get(key))) {
                throw new ValidationException(String.format("%s, Cannot be Changes as Package, Weight and Volume Details Update not allowed in Upload. for container GUID: %s", key, containerId));
            }
        }
    }
    public Map<UUID, Map<String, Object>> validationContainerUploadInShipment(List<Containers> consolContainers) {
        Map<UUID, Map<String, Object>> map = new HashMap<>();
        for (Containers containers : consolContainers) {
            map.putIfAbsent(containers.getGuid(), new HashMap<>());
            map.get(containers.getGuid()).put("grossVolume", containers.getGrossVolume());
            map.get(containers.getGuid()).put("grossVolumeUnit", containers.getGrossVolumeUnit());
            map.get(containers.getGuid()).put("grossWeight", containers.getGrossWeight());
            map.get(containers.getGuid()).put("grossWeightUnit", containers.getGrossWeightUnit());
            map.get(containers.getGuid()).put("packs", containers.getPacks());
            map.get(containers.getGuid()).put("packsType", containers.getPacksType());
        }
        return map;
    }
    public static void setShipmentOrConsoleId(BulkUploadRequest request, String module, List<ContainerV3Request> requests) {
        requests.forEach(p -> {
            if (module.equalsIgnoreCase(SHIPMENT)) {
                p.setShipmentsId(request.getShipmentId());
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

    public void validateHsCode(List<Containers> containersList) {
        if (!containersList.isEmpty()) {
            Set<String> validHsCode = getValidHsCodes(syncCommodityAndHsCode(containersList));
            for (int i = 0; i < containersList.size(); i++) {
                String hsCode = containersList.get(i).getHsCode();
                if (StringUtils.isNotBlank(hsCode) && !validHsCode.contains(hsCode)) {
                    throw new ValidationException(String.format(ContainerConstants.HS_CODE_OR_COMMODITY_IS_INVALID, i + 1));
                }
            }
        }
    }

    public static Set<String> syncCommodityAndHsCode(List<Containers> containersList) {
        Set<String> hsCodeList = new HashSet<>();
        for (Containers container : containersList) {
            String hsCode = container.getHsCode();
            String commodityCode = container.getCommodityCode();
            if (StringUtils.isBlank(commodityCode) && StringUtils.isNotBlank(hsCode)) {
                container.setCommodityCode(hsCode);
            } else if (StringUtils.isBlank(hsCode) && StringUtils.isNotBlank(commodityCode)) {
                container.setHsCode(commodityCode);
            }
            if (StringUtils.isNotBlank(container.getHsCode())) {
                hsCodeList.add(container.getHsCode());
            }
        }
        return hsCodeList;
    }
    public Integer getHsCodeBatchProcessLimit(){
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
}
