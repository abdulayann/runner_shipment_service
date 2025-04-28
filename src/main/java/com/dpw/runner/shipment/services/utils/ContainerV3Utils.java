package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletResponse;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Slf4j
@Component
public class ContainerV3Utils {

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

}
