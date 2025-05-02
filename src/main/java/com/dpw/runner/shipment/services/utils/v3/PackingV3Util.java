package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.ExcelCell;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.nimbusds.jose.util.Pair;
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

import javax.servlet.http.HttpServletResponse;
import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CONTENT_TYPE_FOR_EXCEL;
import static com.dpw.runner.shipment.services.commons.constants.Constants.YYYY_MM_DD_HH_MM_SS_FORMAT;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Slf4j
@Component
public class PackingV3Util {

    @Autowired
    private IPackingDao packingDao;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private MasterDataUtils masterDataUtils;

    private List<String> columnsSequenceForExcelDownloadForCargo = List.of(
            "guid", "shipmentNumber", "packs", "packsType", "innerPackageNumber", "innerPackageType", "origin", "packingOrder",
            "length", "lengthUnit", "width", "widthUnit", "height", "heightUnit", "weight", "weightUnit", "volume", "volumeUnit",
            "netWeight", "netWeightUnit", "chargeable", "chargeableUnit", "marksnNums", "countryCode", "goodsDescription",
            "referenceNumber", "inspections", "DGClass", "DGSubstanceId", "flashPoint", "UNDGContact", "isTemperatureControlled",
            "minTemp", "minTempUnit", "maxTemp", "maxTempUnit", "commodity", "HSCode", "customsReleaseCode", "unNumberAir", "dgClassAir",
            "dgClassAirDescription"
    );

    public void downloadPacking(HttpServletResponse response, BulkDownloadRequest request) throws RunnerException {
        try {
            List<Packing> result = new ArrayList<>();
            if (request.getShipmentId() != null) {
                ListCommonRequest req = constructListCommonRequest("shipmentId", Long.valueOf(request.getShipmentId()), "=");
                Pair<Specification<Packing>, Pageable> pair = fetchData(req, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                List<Packing> packingList = packings.getContent();
                result.addAll(packingList);
            }

            if (request.getConsolidationId() != null) {
                ListCommonRequest req2 = constructListCommonRequest("consolidationId", Long.valueOf(request.getConsolidationId()), "=");
                Pair<Specification<Packing>, Pageable> pair = fetchData(req2, Packing.class);
                Page<Packing> packings = packingDao.findAll(pair.getLeft(), pair.getRight());
                List<Packing> packingList = packings.getContent();
                if (result.isEmpty()) {
                    result.addAll(packingList);
                } else {
                    result = result.stream().filter(result::contains).toList();
                }
            }
            LocalDateTime currentTime = LocalDateTime.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern(YYYY_MM_DD_HH_MM_SS_FORMAT);
            String timestamp = currentTime.format(formatter);
            String filenameWithTimestamp = "CargoDetails_" + timestamp + Constants.XLSX;

            try(XSSFWorkbook workbook = new XSSFWorkbook()) {
                XSSFSheet sheet = workbook.createSheet("CargoDetails");

                List<PackingExcelModel> modelList = commonUtils.convertToList(result, PackingExcelModel.class);
                convertModelToExcel(modelList, sheet);

                response.setContentType(CONTENT_TYPE_FOR_EXCEL);
                response.setHeader("Content-Disposition", "attachment; filename=" + filenameWithTimestamp);

                try (var outputStream = response.getOutputStream()) {
                    workbook.write(outputStream);
                }
            }

        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    private void convertModelToExcel(List<PackingExcelModel> modelList, XSSFSheet sheet) throws IllegalAccessException {

        // Create header row using annotations for order
        Row headerRow = sheet.createRow(0);
        Field[] fields = PackingExcelModel.class.getDeclaredFields();

        Map<String, Field> fieldNameMap = Arrays.stream(fields).filter(f->f.isAnnotationPresent(ExcelCell.class)).collect(Collectors.toMap(Field::getName, c-> c));
        columnsToIgnore(fieldNameMap);

        processOriginFieldNameMap(modelList, fieldNameMap);
        List<Field> fieldsList = reorderFields(fieldNameMap, columnsSequenceForExcelDownloadForCargo);
        int i = 0;
        for (var field : fieldsList){
            Cell cell = headerRow.createCell(i++);
            cell.setCellValue(!field.getAnnotation(ExcelCell.class).displayName().isEmpty() ? field.getAnnotation(ExcelCell.class).displayName() : field.getName());
        }

        // Populate data
        int rowIndex = 1;
        for (PackingExcelModel model : modelList) {
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

    private void processOriginFieldNameMap(List<PackingExcelModel> modelList, Map<String, Field> fieldNameMap) {
        if(fieldNameMap.containsKey("Origin")) {
            Set<String> unlocationsRefGuids = new HashSet<>();
            for (PackingExcelModel model : modelList){
                unlocationsRefGuids.add(model.getOrigin());
            }
            if(!unlocationsRefGuids.isEmpty()) {
                Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(unlocationsRefGuids, EntityTransferConstants.LOCATION_SERVICE_GUID);
                for (PackingExcelModel model : modelList){
                    if(keyMasterDataMap.containsKey(model.getOrigin())){
                        var locCode = keyMasterDataMap.get(model.getOrigin()).LocCode;
                        model.setOrigin(locCode);
                    }
                }
            }
        }
    }

    private void columnsToIgnore(Map<String, Field> fieldNameMap) {
        for(var field : Constants.ColumnsToBeDeletedForConsolidationCargo) {
            if (fieldNameMap.containsKey(field)) {
                fieldNameMap.remove(field);
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

    public void addAllMasterDataInSingleCallList(List<PackingResponse> packingListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<MasterListRequest> listRequests = new HashSet<>();

            packingListResponse.forEach(pack -> listRequests.addAll(masterDataUtils.createInBulkMasterListRequest(pack, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + pack.getId(), cacheMap)));

            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(listRequests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList(MasterDataConstants.ITEM_TYPE, MasterDataConstants.ITEM_VALUE, MasterDataConstants.ITEM_DESCRIPTION, MasterDataConstants.VALUE_N_DESC, MasterDataConstants.CASCADE));

            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            Set<String> keys = new HashSet<>();
            commonUtils.createMasterDataKeysList(listRequests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), cacheMap);

            packingListResponse.forEach(pack -> pack.setMasterData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + pack.getId()), CacheConstants.MASTER_LIST, cacheMap)));

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllMasterDataInSingleCallPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllUnlocationInSingleCallList(List<PackingResponse> packingListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> locationCodes = new HashSet<>();

            packingListResponse.forEach(pack -> locationCodes.addAll(masterDataUtils.createInBulkUnLocationsRequest(pack, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + pack.getId(), cacheMap)));

            Map<String, EntityTransferUnLocations> keyMasterDataMap = masterDataUtils.fetchInBulkUnlocations(locationCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.UNLOCATIONS, locationCodes, new EntityTransferUnLocations(), cacheMap);

            packingListResponse.forEach(pack -> pack.setUnlocationData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + pack.getId()), CacheConstants.UNLOCATIONS, cacheMap)));

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(keyMasterDataMap));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllUnlocationInSingleCallListPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

    public void addAllCommodityTypesInSingleCall(List<PackingResponse> packingListResponse) {
        try {
            Map<String, Object> cacheMap = new HashMap<>();
            Map<String, Map<String, String>> fieldNameKeyMap = new HashMap<>();
            Set<String> commodityTypes = new HashSet<>();
            packingListResponse.forEach(pack -> commodityTypes.addAll(masterDataUtils.createInBulkCommodityTypeRequest(pack, Packing.class, fieldNameKeyMap, Packing.class.getSimpleName() + pack.getId(), cacheMap)));

            Map<String, EntityTransferCommodityType> v1Data = masterDataUtils.fetchInBulkCommodityTypes(commodityTypes.stream().toList());
            masterDataUtils.pushToCache(v1Data, CacheConstants.COMMODITY, commodityTypes, new EntityTransferCommodityType(), cacheMap);

            packingListResponse.forEach(pack -> pack.setCommodityTypeData(masterDataUtils.setMasterData(fieldNameKeyMap.get(Packing.class.getSimpleName() + pack.getId()), CacheConstants.COMMODITY, cacheMap)));

            CompletableFuture.completedFuture(ResponseHelper.buildSuccessResponse(v1Data));
        } catch (Exception ex) {
            log.error("Request: {} | Error Occurred in CompletableFuture: addAllCommodityTypesInSingleCallListPacksList in class: {} with exception: {}", LoggerHelper.getRequestIdFromMDC(), Packing.class.getSimpleName(), ex.getMessage());
            CompletableFuture.completedFuture(null);
        }
    }

}
