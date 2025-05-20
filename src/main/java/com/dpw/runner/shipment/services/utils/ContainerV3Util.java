package com.dpw.runner.shipment.services.utils;

import static com.dpw.runner.shipment.services.commons.constants.PackingConstants.PKG;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.response.ContainerBaseResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.nimbusds.jose.util.Pair;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.ModelAttribute;

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

    public void addAllUnlocationInSingleCallList(List<ContainerBaseResponse> containers, Map<String, Object> masterDataResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();

            containers.forEach(container -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(container, Containers.class, fieldNameKeyMap, Containers.class.getSimpleName() + container.getId(), cacheMap)));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            if (masterDataResponse == null) {
                containers.forEach(container -> container.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + container.getId()), CacheConstants.UNLOCATIONS, cacheMap)));
            }else{
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.UNLOCATIONS, masterDataResponse, cacheMap);
            }
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

            if (masterDataResponse == null) {
                containers.forEach(container -> container.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + container.getId()), CacheConstants.COMMODITY, cacheMap)));
            }else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.COMMODITY, masterDataResponse, cacheMap);
            }
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

            if (masterDataResponse == null) {
                containers.forEach(container -> container.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Containers.class.getSimpleName() + container.getId()), CacheConstants.MASTER_LIST, cacheMap)));
            }else {
                masterDataKeyUtils.setMasterDataValue(fieldNameKeyMap, CacheConstants.MASTER_LIST, masterDataResponse, cacheMap);
            }
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
            if(container.getNetWeight() == null)
                container.setNetWeight(BigDecimal.ZERO);
            if(isStringNullOrEmpty(container.getNetWeightUnit())) {
                container.setNetWeightUnit(
                        isStringNullOrEmpty(container.getGrossWeightUnit()) ?
                                commonUtils.getShipmentSettingFromContext().getWeightChargeableUnit() : container.getGrossWeightUnit());
            }
            if(container.getGrossWeight() == null || BigDecimal.ZERO.equals(container.getGrossWeight()) || isStringNullOrEmpty(container.getGrossWeightUnit())) {
                container.setNetWeight(BigDecimal.ZERO);
                return;
            }
            container.setNetWeight(getAddedWeight(container.getNetWeight(), container.getNetWeightUnit(), container.getTareWeight(), container.getTareWeightUnit()));
            container.setNetWeight(getAddedWeight(container.getNetWeight(), container.getNetWeightUnit(), container.getGrossWeight(), container.getGrossWeightUnit()));
        }
    }

    public void resetContainerDataForRecalculation(Containers container) {
        container.setNetWeight(BigDecimal.ZERO);
        container.setGrossVolume(BigDecimal.ZERO);
        container.setGrossWeight(BigDecimal.ZERO);
        container.setPacks("0");
        container.setPacksType(null);
    }

    public void addNoOfPackagesToContainerFromShipment(Containers container, String packs, String packsType) {
        if(isStringNullOrEmpty(packs))
            return;
        addNoOfPackagesToContainerFromShipment(container, Integer.parseInt(packs), packsType);
    }

    public void addNoOfPackagesToContainerFromShipment(Containers container, Integer packs, String packsType) {
        if(isStringNullOrEmpty(packsType) || packs == null || packs == 0)
            return;
        if(isStringNullOrEmpty(container.getPacks()))
            container.setPacks("0");
        if(isStringNullOrEmpty(container.getPacksType()))
            container.setPacksType(packsType);
        else if(!Objects.equals(packsType, container.getPacksType()))
            container.setPacksType(PKG);
        container.setPacks(String.valueOf(Integer.parseInt(container.getPacks()) + packs));
    }

}
