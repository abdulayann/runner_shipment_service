package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.ReportingService.Reports.IReport;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.commons.requests.BulkUploadRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.*;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.AllocationsRequest;
import com.dpw.runner.shipment.services.dto.request.AutoCalculatePackingRequest;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.syncing.Entity.BulkPackingRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.*;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Service
public class PackingService implements IPackingService {
    @Autowired
    IPackingDao packingDao;
    @Autowired
    IContainerDao containersDao;
    @Autowired
    private IAuditLogService auditLogService;
    private List<String> columnsSequenceForExcelDownloadForCargo = List.of(
            "guid", "shipmentNumber", "packs", "packsType", "innerPackageNumber", "innerPackageType", "origin", "packingOrder",
            "length", "lengthUnit", "width", "widthUnit", "height", "heightUnit", "weight", "weightUnit", "volume", "volumeUnit",
            "netWeight", "netWeightUnit", "chargeable", "chargeableUnit", "marksnNums", "countryCode", "goodsDescription",
            "referenceNumber", "inspections", "DGClass", "DGSubstanceId", "flashPoint", "UNDGContact", "isTemperatureControlled",
            "minTemp", "minTempUnit", "maxTemp", "maxTempUnit", "commodity", "HSCode", "customsReleaseCode", "unNumberAir", "dgClassAir",
            "dgClassAirDescription"
    );
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IConsolidationDetailsDao consolidationDao;
    @Autowired
    @Lazy
    private IConsolidationService consolidationService;
    @Autowired
    private IContainerService containerService;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private IPackingSync packingSync;
    @Autowired
    private CSVParsingUtil<Packing> parser;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;
    @Autowired
    private SyncConfig syncConfig;

    private static void applyHazardousValidation(Set<String> hazardousClassMasterData,
                                                 Map<Long, Long> dicDGSubstanceUNDGContact, Map<Long, String> dicDGSubstanceFlashPoint
            , int row, Packing packingRow) {
        Boolean isHazardous = packingRow.getHazardous();
        if (isHazardous != null && isHazardous) {

            boolean dgUser = UserContext.isAirDgUser();
            if(!dgUser)
                throw new ValidationException("You do not have Air DG permissions for this.");

            // DG CLASS(HAZARDOUS CLASS)
            validateDgClass(hazardousClassMasterData, row, packingRow);

            valiadteDGSubstanceId(dicDGSubstanceUNDGContact, row, packingRow);

            validateFlashPoint(dicDGSubstanceFlashPoint, row, packingRow);

            validateUNDGContact(dicDGSubstanceUNDGContact, row, packingRow);
        }
    }

    private static void validateDgClass(Set<String> hazardousClassMasterData, int row, Packing packingRow) {
        if (!StringUtils.isEmpty(packingRow.getDGClass())) {
            String dgClass = packingRow.getDGClass();
                if (hazardousClassMasterData != null && !hazardousClassMasterData.contains(dgClass)) {
                    throw new ValidationException("DG class is invalid at row: " + row);
                }
        }
    }

    private static void valiadteDGSubstanceId(Map<Long, Long> dicDGSubstanceUNDGContact, int row, Packing packingRow) {
        if (packingRow.getDGSubstanceId() != null) {
            if (!dicDGSubstanceUNDGContact.containsKey(packingRow.getDGSubstanceId())) {
                throw new ValidationException("DG Substance Id is invalid at row: " + row);
            }
        }
    }

    private static void validateFlashPoint(Map<Long, String> dicDGSubstanceFlashPoint, int row, Packing packingRow) {
        if (!StringUtils.isEmpty(packingRow.getFlashPoint())) {
            if (packingRow.getDGSubstanceId() != null) {
                if (!dicDGSubstanceFlashPoint.containsKey(Long.valueOf(packingRow.getDGSubstanceId())) ||
                        !Objects.equals(dicDGSubstanceFlashPoint.get(Long.valueOf(packingRow.getDGSubstanceId())), packingRow.getFlashPoint())) {
                    throw new ValidationException(PackingConstants.FLASH_POINT_INVALID_ERROR + row);
                }
            } else {
                throw new ValidationException(PackingConstants.FLASH_POINT_INVALID_ERROR + row);
            }
        }
    }

    private static void validateUNDGContact(Map<Long, Long> dicDGSubstanceUNDGContact, int row, Packing packingRow) {
        if (!StringUtils.isEmpty(packingRow.getUNDGContact())) {
            if (packingRow.getDGSubstanceId() != null) {
                long substanceId = packingRow.getDGSubstanceId();
                if (!dicDGSubstanceUNDGContact.containsKey(substanceId) ||
                        !Objects.equals(dicDGSubstanceUNDGContact.get(Long.valueOf(packingRow.getDGSubstanceId())), Long.valueOf(packingRow.getUNDGContact()))) {
                    throw new ValidationException("UNDGContact is invalid at row: " + row);
                }
            } else if (packingRow.getDGSubstanceId() == null && !StringUtils.isEmpty(packingRow.getUNDGContact())) {
                throw new ValidationException("UNDGContact is invalid at row: " + row);
            }
        }
    }

    private static void applyCommodityTypeValidation(Set<String> dicCommodityType, int row, Packing packingRow) {
        String commodityType = packingRow.getCommodity();
        commodityType = commodityType == null ? StringUtils.EMPTY : commodityType.trim();
        if (!StringUtils.isEmpty(commodityType)) {
            if (dicCommodityType != null && dicCommodityType.contains(commodityType) == false) {
                throw new ValidationException("Commodity Type " + commodityType + " is not valid at row " + row);
            }
        }
    }

    private static Containers addWeightVolume(Packing request, Containers newContainer) throws RunnerException {
        if(newContainer != null && request != null) {
            if(IsStringNullOrEmpty(newContainer.getAchievedWeightUnit()))
                newContainer.setAchievedWeightUnit(newContainer.getAllocatedWeightUnit());
            if(IsStringNullOrEmpty(newContainer.getAchievedVolumeUnit()))
                newContainer.setAchievedVolumeUnit(newContainer.getAllocatedVolumeUnit());
            BigDecimal finalWeight = new BigDecimal(convertUnit(Constants.MASS, request.getWeight(), request.getWeightUnit(), newContainer.getAchievedWeightUnit()).toString());
            BigDecimal finalVolume = new BigDecimal(convertUnit(VOLUME, request.getVolume(), request.getVolumeUnit(), newContainer.getAchievedVolumeUnit()).toString());
            if(newContainer.getAchievedWeight() != null)
                finalWeight = finalWeight.add(newContainer.getAchievedWeight());
            if(newContainer.getAchievedVolume() != null)
                finalVolume = finalVolume.add(newContainer.getAchievedVolume());
            newContainer.setAchievedWeight(finalWeight);
            newContainer.setAchievedVolume(finalVolume);
        }
        return newContainer;
    }

    private static Containers subtractWeightVolume(Packing request, Containers oldContainer) throws RunnerException {
        if(oldContainer != null && request != null) {
            if(IsStringNullOrEmpty(oldContainer.getAchievedWeightUnit()))
                oldContainer.setAchievedWeightUnit(oldContainer.getAllocatedWeightUnit());
            if(IsStringNullOrEmpty(oldContainer.getAchievedVolumeUnit()))
                oldContainer.setAchievedVolumeUnit(oldContainer.getAllocatedVolumeUnit());
            BigDecimal finalWeight = new BigDecimal(convertUnit(Constants.MASS, request.getWeight(), request.getWeightUnit(), oldContainer.getAchievedWeightUnit()).toString());
            BigDecimal finalVolume = new BigDecimal(convertUnit(VOLUME, request.getVolume(), request.getVolumeUnit(), oldContainer.getAchievedVolumeUnit()).toString());
            if(oldContainer.getAchievedWeight() != null) {
                finalWeight = oldContainer.getAchievedWeight().subtract(finalWeight);
                oldContainer.setAchievedWeight(finalWeight);
            }
            if(oldContainer.getAchievedVolume() != null) {
                finalVolume = oldContainer.getAchievedVolume().subtract(finalVolume);
                oldContainer.setAchievedVolume(finalVolume);
            }
        }
        return oldContainer;
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public void uploadPacking(BulkUploadRequest request) throws RunnerException {
        if (request.getConsolidationId() == null) {
            throw new ValidationException("Please save the consolidation and then try again.");
        }

        Map<Long, Long> dicDGSubstanceUNDGContact = new HashMap<>();
        Map<Long, String> dicDGSubstanceFlashPoint = new HashMap<>();
        Map<String, Set<String>> masterDataMap = new HashMap<>();
        Map<String, String> locCodeToLocationReferenceGuidMap = new HashMap<>();
        List<Packing> packingList = new ArrayList<>();
        try {
            packingList = parser.parseExcelFile(request.getFile(), request, null, masterDataMap, Packing.class, PackingExcelModel.class, dicDGSubstanceUNDGContact, dicDGSubstanceFlashPoint, locCodeToLocationReferenceGuidMap);
            packingList.stream().forEach(packing -> packing.setConsolidationId(request.getConsolidationId()));
            var utilizationResponse = calculatePacksUtilisationForConsolidation(CalculatePackUtilizationRequest.builder()
                .consolidationId(request.getConsolidationId())
                .packingList(jsonHelper.convertValueToList(packingList, PackingRequest.class))
                .build());

            validateConsolidationAchievedQuantities(request, utilizationResponse);
        }
        catch(IOException | ValidationException e) {
            throw new RunnerException(e.getMessage());
        }

        applyPackingValidations(packingList, request, masterDataMap, dicDGSubstanceUNDGContact, dicDGSubstanceFlashPoint);

        packingList = packingDao.saveAll(packingList);
        packingSync.sync(packingList, request.getConsolidationId(), request.getShipmentId());
    }

    private void validateConsolidationAchievedQuantities(BulkUploadRequest request, PackSummaryResponse utilizationResponse) {
        if(utilizationResponse != null && utilizationResponse.getConsolidationAchievedQuantities() != null) {
            Double wtUtilization = utilizationResponse.getConsolidationAchievedQuantities().getWeightUtilization() != null ? Double.valueOf(utilizationResponse.getConsolidationAchievedQuantities().getWeightUtilization()) : 0;
            Double volUtilization = utilizationResponse.getConsolidationAchievedQuantities().getVolumeUtilization() != null ? Double.valueOf(utilizationResponse.getConsolidationAchievedQuantities().getVolumeUtilization()) : 0;
            if(wtUtilization >= 100) {
                if(!Boolean.TRUE.equals(request.getOverride()))
                    throw new ValidationException((String.format("Entered Pack weight %s exceeds the allocated weight %s", utilizationResponse.getAchievedWeight(), utilizationResponse.getAllocatedWeight())));
            }
            else if(volUtilization >= 100) {
                if(!Boolean.TRUE.equals(request.getOverride()))
                    throw new ValidationException(String.format("Entered Pack Volume %s exceeds the allocated volume %s", utilizationResponse.getAchievedVolume(), utilizationResponse.getAllocatedVolume()));
            }

        }
    }

    private void applyPackingValidations(List<Packing> packingList, BulkUploadRequest request, Map<String, Set<String>> masterDataMap,
                                         Map<Long, Long> dicDGSubstanceUNDGContact, Map<Long, String> dicDGSubstanceFlashPoint
    ) throws RunnerException {
        String transportMode = request.getTransportMode();
        Set<String> dicCommodityType = masterDataMap.get("CommodityCodes");
        Set<String> hazardousClassMasterData = masterDataMap.get(MasterDataType.DG_CLASS.getDescription());
        for (int row = 0; row < packingList.size(); row++) {
            Packing packingRow = packingList.get(row);
            checkCalculatedVolumeAndActualVolume(row + 1, packingRow);
            applyChargeableValidation(transportMode, row + 1, packingRow, masterDataMap);
            applyCommodityTypeValidation(dicCommodityType, row + 1, packingRow);
            applyVolumetricWeightValidation(row + 1, packingRow);
            applyHazardousValidation(hazardousClassMasterData, dicDGSubstanceUNDGContact, dicDGSubstanceFlashPoint, row + 1, packingRow);
            if (!StringUtils.isEmpty(packingRow.getFlashPoint()) && packingRow.getDGSubstanceId() == null) {
                throw new ValidationException(PackingConstants.FLASH_POINT_INVALID_ERROR + row + 1);
            }
        }
    }

    private void applyVolumetricWeightValidation(int i, Packing packingRow) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getVolumeWeightUnit())) {
            if (packingRow.getVolumeWeight() != null) {
                String wtunit = packingRow.getWeightUnit() != null ? packingRow.getWeightUnit() : WEIGHT_UNIT_KG;
                var vwob = getVolumeWeightChargeable(packingRow);
                validateVolumeWeight(i, packingRow, vwob, wtunit);
            }
        } else if (packingRow.getVolumeWeight() != null) {
            throw new ValidationException("Volumetric weight unit is empty or Volumetric weight unit not entered at row: " + i);
        }
    }

    private void validateVolumeWeight(int i, Packing packingRow, VolumeWeightChargeable vwob, String wtunit) {
        if (vwob.getVolumeWeight() != null) {
            var calculatedVolumeWeight = vwob.getVolumeWeight();
            calculatedVolumeWeight = calculatedVolumeWeight.setScale(2, RoundingMode.HALF_UP);
            var actualVolumeWeight = packingRow.getVolumeWeight();
            actualVolumeWeight = actualVolumeWeight.setScale(2, RoundingMode.HALF_UP);
            if (!wtunit.equals(vwob.getVolumeWeightUnit())) {
                throw new ValidationException("Volumetric weight unit not in " + wtunit + " at row: " + i);
            }
            if (calculatedVolumeWeight.compareTo(actualVolumeWeight) != 0) { // not equal
                throw new ValidationException("Volumetric weight is invalid at row: " + i);
            }
        }
    }

    private void checkCalculatedVolumeAndActualVolume(int row, Packing packingRow) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getVolumeUnit())) {
            if (packingRow.getVolume() != null) {
                if (!Objects.equals(packingRow.getVolumeUnit(), VOLUME_UNIT_M3)) {
                    throw new ValidationException("Volume unit not in M3 at row: " + row);
                }
                BigDecimal actualVolume = packingRow.getVolume();
                actualVolume = actualVolume.setScale(2, BigDecimal.ROUND_HALF_UP);
                var calculatedVolume = getCalculatedVolume(packingRow);
                calculatedVolume = calculatedVolume.setScale(2, BigDecimal.ROUND_HALF_UP);
                if (actualVolume.compareTo(calculatedVolume) != 0) { // not equal
                    throw new ValidationException("Volume is invalid at row: " + row);
                }
            }
        } else if (packingRow.getVolume() != null) {
            throw new ValidationException("Volume unit is empty or Volume unit not entered at row: " + row);
        }
    }

    private BigDecimal getCalculatedVolume(Packing packingRow) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getPacks()) && packingRow.getLength() != null
                && packingRow.getHeight() != null && packingRow.getWidth() != null
                && !StringUtils.isEmpty(packingRow.getWidthUnit()) && !StringUtils.isEmpty(packingRow.getHeightUnit())
                && !StringUtils.isEmpty(packingRow.getHeightUnit())) {
            String lengthUnit = packingRow.getLengthUnit().equals(FT) ? FOOT_FT : packingRow.getLengthUnit();
            String widthUnit = packingRow.getWidthUnit().equals(FT) ? FOOT_FT : packingRow.getWidthUnit();
            String heightUnit = packingRow.getHeightUnit().equals(FT) ? FOOT_FT : packingRow.getHeightUnit();
            var length = new BigDecimal(convertUnit(LENGTH, packingRow.getLength(), lengthUnit, M).doubleValue());
            var height = new BigDecimal(convertUnit(LENGTH, packingRow.getHeight(), heightUnit, M).doubleValue());
            var width = new BigDecimal(convertUnit(LENGTH, packingRow.getWidth(), widthUnit, M).doubleValue());
            int packs = Integer.parseInt(packingRow.getPacks());
            return length.multiply(width).multiply(height).multiply(BigDecimal.valueOf(packs));
        }
        return BigDecimal.ZERO;
    }

    private void applyChargeableValidation(String transportMode, int row, Packing packingRow, Map<String, Set<String>> masterDataMap) throws RunnerException {
        if (!StringUtils.isEmpty(packingRow.getChargeableUnit())) {
            if (masterDataMap.containsKey(MasterDataType.WEIGHT_UNIT.getDescription()) &&
                    !masterDataMap.get(MasterDataType.WEIGHT_UNIT.getDescription()).contains(packingRow.getChargeableUnit()))
                throw new ValidationException("Chargeable unit is invalid at row: " + row);
        }
        if (packingRow.getChargeableUnit() != null && !packingRow.getChargeableUnit().isEmpty() &&
                transportMode != null && transportMode.equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR) &&
                packingRow.getChargeable() != null) {
            if (!packingRow.getChargeableUnit().equals(Constants.WEIGHT_UNIT_KG)) {
                throw new ValidationException("Chargeable unit not in KG at row: " + row);
            }
            var actualChargeable = packingRow.getChargeable();
            actualChargeable = actualChargeable.setScale(2, BigDecimal.ROUND_HALF_UP);
            BigDecimal calculatedChargeable = null;

            var vwob = getVolumeWeightChargeable(packingRow);
            validateChargeable(row, vwob, actualChargeable);
        } else if (packingRow.getChargeable() != null && StringUtils.isEmpty(packingRow.getChargeableUnit())) {
            throw new ValidationException("Chargeable unit is empty or Chargeable unit not entered at row: " + row);
        }
    }

    private void validateChargeable(int row, VolumeWeightChargeable vwob, BigDecimal actualChargeable) {
        BigDecimal calculatedChargeable;
        if (vwob.getChargeable() != null) {
            calculatedChargeable = vwob.getChargeable();
            calculatedChargeable = calculatedChargeable.setScale(2, BigDecimal.ROUND_HALF_UP);
            if (!Objects.equals(calculatedChargeable, actualChargeable)) {
                BigDecimal difference = calculatedChargeable.subtract(actualChargeable).abs();
                BigDecimal threshold = new BigDecimal("0.01");
                if (difference.compareTo(threshold) > 0) {
                    throw new ValidationException("Chargeable is invalid at row: " + row);
                }
            }
        }
    }

    private VolumeWeightChargeable getVolumeWeightChargeable(Packing packingRow) throws RunnerException {
        return consolidationService.calculateVolumeWeight(Constants.TRANSPORT_MODE_AIR,
                packingRow.getWeightUnit() == null ? Constants.WEIGHT_UNIT_KG : packingRow.getWeightUnit(),
                packingRow.getVolumeUnit() == null ? Constants.VOLUME_UNIT_M3 : packingRow.getVolumeUnit(),
                packingRow.getWeight() == null ? BigDecimal.ZERO : packingRow.getWeight(),
                packingRow.getVolume() == null ? BigDecimal.ZERO : packingRow.getVolume());
    }

    @Override
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
                convertModelToExcel(modelList, sheet, request);

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

    private void convertModelToExcel(List<PackingExcelModel> modelList, XSSFSheet sheet, BulkDownloadRequest request) throws IllegalAccessException {

        // Create header row using annotations for order
        Row headerRow = sheet.createRow(0);
        Field[] fields = PackingExcelModel.class.getDeclaredFields();
//        Arrays.sort(fields, Comparator.comparingInt(f -> f.getAnnotation(ExcelCell.class).order()));

        Map<String, Field> fieldNameMap = Arrays.stream(fields).filter(f->f.isAnnotationPresent(ExcelCell.class)).collect(Collectors.toMap(Field::getName, c-> c));
        ColumnsToIgnore(fieldNameMap, request);

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

    private void ColumnsToIgnore(Map<String, Field> fieldNameMap, BulkDownloadRequest request) {
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

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        return null;
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Packing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<Packing>, Pageable> tuple = fetchData(request, Packing.class);
            Page<Packing> packingPage = packingDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Packing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(packingPage.getContent()),
                    packingPage.getTotalPages(),
                    packingPage.getTotalElements());
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
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> calculateWeightVolumne(CommonRequestModel commonRequestModel) throws RunnerException {
        PackContainerNumberChangeRequest request = (PackContainerNumberChangeRequest) commonRequestModel.getData();
        PackContainerNumberChangeResponse response = new PackContainerNumberChangeResponse();
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        if(request.getNewContainer() == null && request.getOldContainer() == null) {
            return ResponseHelper.buildSuccessResponse(response);
        }
        ShipmentDetails shipmentDetails = null;
        try {
            if(request.getNewPack() != null)
                shipmentDetails = shipmentDao.findById(request.getNewPack().getShipmentId()).get();
            else if(request.getOldPack() != null)
                shipmentDetails = shipmentDao.findById(request.getOldPack().getShipmentId()).get();
        }
        catch (Exception e) {
            throw new RunnerException("Please send correct shipment Id in packing request");
        }
        Containers oldContainer = null;
        if(request.getOldContainer() != null)
            oldContainer = jsonHelper.convertValue(request.getOldContainer(), Containers.class);
        Containers newContainer = null;
        if(request.getNewContainer() != null)
            newContainer = jsonHelper.convertValue(request.getNewContainer(), Containers.class);
        processContainersInResponse(shipmentSettingsDetails, shipmentDetails, request, oldContainer, newContainer, response);
        return ResponseHelper.buildSuccessResponse(response);
    }

    private void processContainersInResponse(ShipmentSettingsDetails shipmentSettingsDetails, ShipmentDetails shipmentDetails, PackContainerNumberChangeRequest request, Containers oldContainer, Containers newContainer, PackContainerNumberChangeResponse response) throws RunnerException {
        if(shipmentSettingsDetails.getMultipleShipmentEnabled() != null && shipmentSettingsDetails.getMultipleShipmentEnabled()
        && !shipmentDetails.getShipmentType().equals(Constants.CARGO_TYPE_FCL)
                && (shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA) || shipmentDetails.getTransportMode().equals(Constants.TRANSPORT_MODE_ROA))) {

            Packing newPacking = jsonHelper.convertValue(request.getNewPack(), Packing.class);
            Packing oldPacking = newPacking;
            if(request.getOldPack() != null)
                oldPacking = jsonHelper.convertValue(request.getOldPack(), Packing.class);

            if(request.getNewPack() == null) { // delete pack scenario
                oldContainer = subtractWeightVolume(oldPacking, oldContainer);
            }
            else {
                if(request.getOldContainer() != null && request.getNewContainer() != null && Objects.equals(request.getOldContainer().getId(), request.getNewContainer().getId())) {
                    newContainer = subtractWeightVolume(oldPacking, newContainer);
                    oldContainer = null;
                }
                else {
                    oldContainer = subtractWeightVolume(oldPacking, oldContainer);
                }
                newContainer = addWeightVolume(newPacking, newContainer);
            }
        }
        if(oldContainer != null) {
            containerService.calculateUtilization(oldContainer);
            response.setOldContainer(jsonHelper.convertValue(oldContainer, ContainerResponse.class));
        }
        if(newContainer != null) {
            containerService.calculateUtilization(newContainer);
            response.setNewContainer(jsonHelper.convertValue(newContainer, ContainerResponse.class));
        }
    }

    public PackSummaryResponse calculatePackSummary(List<Packing> packingList, String transportMode, String containerCategory, ShipmentMeasurementDetailsDto dto) throws RunnerException {
        try {
            PackSummaryResponse response = new PackSummaryResponse();
            double totalWeight = 0;
            double netWeight = 0;
            StringBuilder packsCount = new StringBuilder();
            double totalVolume = 0;
            double volumetricWeight = 0;
            double chargeableWeight = 0;
            int totalPacks = 0;
            int dgPacks = 0;
            int totalInnerPacks = 0;
            String packsUnit = null;
            String innerPacksUnit = null;
            Map<String, Long> map = new HashMap<>();
            String toWeightUnit = Constants.WEIGHT_UNIT_KG;
            String toVolumeUnit = Constants.VOLUME_UNIT_M3;
            ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
            V1TenantSettingsResponse v1TenantSettingsResponse = commonUtils.getCurrentTenantSettings();
            if(!IsStringNullOrEmpty(shipmentSettingsDetails.getWeightChargeableUnit()))
                toWeightUnit = shipmentSettingsDetails.getWeightChargeableUnit();
            if(!IsStringNullOrEmpty(shipmentSettingsDetails.getVolumeChargeableUnit()))
                toVolumeUnit = shipmentSettingsDetails.getVolumeChargeableUnit();
            if(packingList != null) {
                for (Packing packing: packingList) {
                    double winDef = convertUnit(Constants.MASS, packing.getWeight(), packing.getWeightUnit(), toWeightUnit).doubleValue();
                    double volDef = convertUnit(VOLUME, packing.getVolume(), packing.getVolumeUnit(), toVolumeUnit).doubleValue();
                    double netWtDif = convertUnit(MASS, packing.getNetWeight(), packing.getNetWeightUnit(), toWeightUnit).doubleValue();
                    totalWeight = totalWeight + winDef;
                    totalVolume = totalVolume + volDef;
                    netWeight = netWeight + netWtDif;
                    packsUnit = getPacksUnit(packing, packsUnit, map);
                    if(!IsStringNullOrEmpty(packing.getPacks())) {
                        int packs = Integer.parseInt(packing.getPacks());
                        totalPacks = totalPacks + packs;
                        dgPacks = getDgPacks(packing, map, packs, dgPacks);
                    }
                    totalInnerPacks = getTotalInnerPacks(packing, totalInnerPacks);
                    innerPacksUnit = getInnerPacksUnit(packing, innerPacksUnit);
                }
            }
            double totalVolInM3 = (double) convertUnit(VOLUME, BigDecimal.valueOf(totalVolume), toVolumeUnit, VOLUME_UNIT_M3);
            double totalWtInKG = (double) convertUnit(MASS, BigDecimal.valueOf(totalWeight), toWeightUnit, WEIGHT_UNIT_KG);
            if (Objects.equals(transportMode, Constants.TRANSPORT_MODE_SEA)) {
                volumetricWeight = totalWtInKG/1000;
                chargeableWeight = Math.max(volumetricWeight, totalVolInM3);
            }
            else {
                double factor = AIR_FACTOR_FOR_VOL_WT;
                if (transportMode.equals(Constants.TRANSPORT_MODE_ROA)) {
                    factor = ROAD_FACTOR_FOR_VOL_WT;
                }
                volumetricWeight = totalVolInM3 * factor;
                chargeableWeight = Math.max(volumetricWeight, totalWtInKG);
            }
            List<String> sortedKeys = new ArrayList<>(map.keySet());
            Collections.sort(sortedKeys);
            updatePacksCount(sortedKeys, map, packsCount, v1TenantSettingsResponse);
            response.setDgPacks(dgPacks);
            response.setTotalPacksWithUnit(totalPacks + " " + (packsUnit != null? packsUnit : ""));
            response.setTotalPacks(packsCount.toString());
            response.setTotalPacksWeight(String.format(Constants.STRING_FORMAT, IReport.ConvertToWeightNumberFormat(BigDecimal.valueOf(totalWeight), v1TenantSettingsResponse), toWeightUnit));
            response.setTotalPacksVolume(String.format(Constants.STRING_FORMAT, IReport.ConvertToVolumeNumberFormat(BigDecimal.valueOf(totalVolume), v1TenantSettingsResponse), toVolumeUnit));
            response.setPacksVolume(BigDecimal.valueOf(totalVolume));
            response.setPacksVolumeUnit(toVolumeUnit);
            setPacksVolumetricWeightInResponse(transportMode, response, volumetricWeight, v1TenantSettingsResponse);

            // setting these fields for UI to consume; Only for Consolidation
            response.setAchievedWeight(BigDecimal.valueOf(totalWeight));
            response.setAchievedVolume(BigDecimal.valueOf(totalVolume));
            response.setWeightUnit(toWeightUnit);
            response.setVolumeUnit(toVolumeUnit);

            dto.setWeight(BigDecimal.valueOf(totalWeight));
            dto.setWeightUnit(toWeightUnit);
            dto.setNetWeight(BigDecimal.valueOf(netWeight));
            dto.setNetWeightUnit(toWeightUnit);
            dto.setVolume(BigDecimal.valueOf(totalVolume));
            dto.setVolumeUnit(toVolumeUnit);
            dto.setNoOfPacks(String.valueOf(totalPacks));
            dto.setPacksUnit(packsUnit);
            dto.setInnerPacks(totalInnerPacks);
            dto.setInnerPackUnit(innerPacksUnit);

            setChargeableWeightAndUnit(transportMode, chargeableWeight, totalVolume, toVolumeUnit, totalWeight, toWeightUnit, response, v1TenantSettingsResponse);
            return response;
        } catch (Exception e) {
            throw new RunnerException(e.getMessage());
        }
    }

    private void setPacksVolumetricWeightInResponse(String transportMode, PackSummaryResponse response, double volumetricWeight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(Objects.equals(transportMode, TRANSPORT_MODE_SEA))
            response.setPacksVolumetricWeight(String.format(Constants.STRING_FORMAT, IReport.ConvertToWeightNumberFormat(BigDecimal.valueOf(volumetricWeight), v1TenantSettingsResponse), VOLUME_UNIT_M3));
        else
            response.setPacksVolumetricWeight(String.format(Constants.STRING_FORMAT, IReport.ConvertToWeightNumberFormat(BigDecimal.valueOf(volumetricWeight), v1TenantSettingsResponse), WEIGHT_UNIT_KG));
    }

    private String getPacksUnit(Packing packing, String packsUnit, Map<String, Long> map) {
        if(!IsStringNullOrEmpty(packing.getPacksType())) {
            packsUnit = getPacksUnit(packing, packsUnit);
            if(!map.containsKey(packing.getPacksType()))
                map.put(packing.getPacksType(), 0L);
        }
        return packsUnit;
    }

    private void setChargeableWeightAndUnit(String transportMode, double chargeableWeight, double totalVolume, String toVolumeUnit, double totalWeight, String toWeightUnit, PackSummaryResponse response, V1TenantSettingsResponse v1TenantSettingsResponse) throws RunnerException {
        String packChargeableWeightUnit = WEIGHT_UNIT_KG;
        if (!IsStringNullOrEmpty(transportMode) && transportMode.equals(Constants.TRANSPORT_MODE_AIR))
            chargeableWeight = roundOffAirShipment(chargeableWeight);
        if (Objects.equals(transportMode, Constants.TRANSPORT_MODE_SEA)) {
            double volInM3 = convertUnit(VOLUME, BigDecimal.valueOf(totalVolume), toVolumeUnit, Constants.VOLUME_UNIT_M3).doubleValue();
            double wtInKg = convertUnit(Constants.MASS, BigDecimal.valueOf(totalWeight), toWeightUnit, Constants.WEIGHT_UNIT_KG).doubleValue();
            chargeableWeight = Math.max(wtInKg / 1000, volInM3);
            packChargeableWeightUnit = Constants.VOLUME_UNIT_M3;
        }
        chargeableWeight = BigDecimal.valueOf(chargeableWeight).setScale(2, RoundingMode.HALF_UP).doubleValue();
        response.setPacksChargeableWeight(String.format(Constants.STRING_FORMAT, IReport.ConvertToWeightNumberFormat(BigDecimal.valueOf(chargeableWeight), v1TenantSettingsResponse), packChargeableWeightUnit));
        response.setChargeableWeight(BigDecimal.valueOf(chargeableWeight));
        response.setPacksChargeableWeightUnit(packChargeableWeightUnit);
    }

    private void updatePacksCount(List<String> sortedKeys, Map<String, Long> map, StringBuilder packsCount, V1TenantSettingsResponse v1TenantSettingsResponse) {
        for (int i = 0; i< sortedKeys.size(); i++) {
            Long value = map.get(sortedKeys.get(i));
            packsCount.append(IReport.GetDPWWeightVolumeFormat(BigDecimal.valueOf(value), 0, v1TenantSettingsResponse)).append(" ").append(sortedKeys.get(i));
            if (i + 1 < sortedKeys.size())
                packsCount.append(", ");
        }
    }

    private int getDgPacks(Packing packing, Map<String, Long> map, int packs, int dgPacks) {
        if(!IsStringNullOrEmpty(packing.getPacksType())) {
            map.put(packing.getPacksType(), map.get(packing.getPacksType()) + packs);
        }
        if(Boolean.TRUE.equals(packing.getHazardous()))
        {
            dgPacks += packs;
        }
        return dgPacks;
    }

    private String getPacksUnit(Packing packing, String packsUnit) {
        if(packsUnit == null)
            packsUnit = packing.getPacksType();
        else if(!packsUnit.equals(packing.getPacksType()))
            packsUnit = MPK;
        return packsUnit;
    }

    private int getTotalInnerPacks(Packing packing, int totalInnerPacks) {
        if(!IsStringNullOrEmpty(packing.getInnerPackageNumber())) {
            int innerPacks = Integer.parseInt(packing.getInnerPackageNumber());
            totalInnerPacks = totalInnerPacks + innerPacks;
        }
        return totalInnerPacks;
    }

    private String getInnerPacksUnit(Packing packing, String innerPacksUnit) {
        if(!IsStringNullOrEmpty(packing.getInnerPackageType())) {
            if(innerPacksUnit == null)
                innerPacksUnit = packing.getInnerPackageType();
            else if(!innerPacksUnit.equals(packing.getInnerPackageType()))
                innerPacksUnit = MPK;
        }
        return innerPacksUnit;
    }

    public void calculateVolume(AutoCalculatePackingRequest request, AutoCalculatePackingResponse pack) throws RunnerException {

        if (IsStringNullOrEmpty(request.getWidthUnit()) || IsStringNullOrEmpty(request.getLengthUnit()) || IsStringNullOrEmpty(request.getHeightUnit()))
            return;
        if (IsStringNullOrEmpty(request.getPacks()) || request.getLength() == null || request.getWidth() == null || request.getHeight() == null)
            return;

        BigDecimal volume = BigDecimal.valueOf(calculateVolume(request));
        Integer volumeDecimalValue = commonUtils.getShipmentSettingFromContext().getVolumeDecimalPlace();
        if(volumeDecimalValue == null)
            volumeDecimalValue = 3;
        volume = volume.setScale(volumeDecimalValue, RoundingMode.HALF_UP);
        pack.setVolumeUnit("M3");
        pack.setVolume(volume);
        request.setVolumeUnit("M3");
        request.setVolume(volume);
    }

    private double calculateVolume(AutoCalculatePackingRequest request) throws RunnerException {
        return (double) convertUnit(LENGTH, request.getLength(), request.getLengthUnit(), M)
                * (double) convertUnit(LENGTH, request.getWidth(), request.getWidthUnit(), M)
                * (double) convertUnit(LENGTH, request.getHeight(), request.getHeightUnit(), M)
                * Double.parseDouble(request.getPacks());
    }



    @Override
    public ResponseEntity<IRunnerResponse> autoCalculatePacksData(CommonRequestModel commonRequestModel) {

        AutoCalculatePackingRequest request = (AutoCalculatePackingRequest) commonRequestModel.getData();
        AutoCalculatePackingResponse response = new AutoCalculatePackingResponse();
        String responseMsg;
        try {
            String transportMode = request.getTransportMode();
            BigDecimal weight = request.getWeight();
            String weightUnit = request.getWeightUnit();
            calculateVolume(request, response);
            BigDecimal volume = request.getVolume();
            String volumeUnit = request.getVolumeUnit();
            VolumeWeightChargeable vwOb;
            if (weightUnit != null && volumeUnit != null) {
                if (Objects.equals(transportMode, TRANSPORT_MODE_AIR)) {
                    weight = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                    vwOb = consolidationService.calculateVolumeWeight(transportMode, Constants.WEIGHT_UNIT_KG, request.getVolumeUnit(), weight, request.getVolume());
                    response.setChargeable(vwOb.getChargeable());
                } else if (Objects.equals(transportMode, Constants.TRANSPORT_MODE_SEA)) {
                    // wtVol is set as wt (in kg) / 1000 - rounding off multiply by 10 then take ceiling then divide by 10
                    // wtVol unit M3
                    // chargeable is set as max of wtVol and vol in M3
                    // chargeable unit is M3
                    volume = new BigDecimal(convertUnit(VOLUME, volume, volumeUnit, Constants.VOLUME_UNIT_M3).toString());
                    weight = new BigDecimal(convertUnit(Constants.MASS, weight, weightUnit, Constants.WEIGHT_UNIT_KG).toString());
                    response.setChargeable(weight.divide(new BigDecimal("1000")).max(volume));
                    vwOb = consolidationService.calculateVolumeWeight(transportMode, Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, weight, volume);
                }else {
                    vwOb = consolidationService.calculateVolumeWeight(transportMode, weightUnit, volumeUnit, weight, volume);
                }

                response.setVolumeWeight(vwOb.getVolumeWeight());
                response.setVolumeWeightUnit(vwOb.getVolumeWeightUnit());
                response.setChargeableUnit(vwOb.getChargeableUnit());
                response.setVolume(volume);
                response.setVolumeUnit(volumeUnit);
            }
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_CALCULATION_ERROR;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private double roundOffAirShipment(double charge) {
        if (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) {
            charge = Math.floor(charge) + 0.5;
        } else {
            charge = Math.ceil(charge);
        }
        return charge;
    }

    @Override
    public ResponseEntity<IRunnerResponse> listPacksToDetach(CommonRequestModel commonRequestModel) throws RunnerException {
        DetachPacksListDto request = (DetachPacksListDto) commonRequestModel.getData();
        if(request == null || request.getContainerId() == null || request.getShipmentId() == null) {
            throw new ValidationException("Either shipmentId or containerId is incorrect!");
        }
        ListCommonRequest listCommonRequest = andCriteria("containerId", request.getContainerId(), "=", null);
        listCommonRequest  = andCriteria("shipmentId", request.getShipmentId(), "=", listCommonRequest);
        if(request.getPageNo() != null)
            listCommonRequest.setPageNo(request.getPageNo());
        if(request.getPageSize() != null)
            listCommonRequest.setPageSize(request.getPageSize());
        return list(CommonRequestModel.buildRequest(listCommonRequest));
    }

    @Override
    public ResponseEntity<IRunnerResponse> V1PackingCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException {
        PackingRequestV2 packingRequestV2 = (PackingRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return ResponseHelper.buildSuccessResponse();
            }
            Optional<Packing> existingPacking = packingDao.findByGuid(packingRequestV2.getGuid());
            Packing packing = modelMapper.map(packingRequestV2, Packing.class);
            if (existingPacking.isPresent()) {
                packing.setId(existingPacking.get().getId());
                packing.setConsolidationId(existingPacking.get().getConsolidationId());
                packing.setShipmentId(existingPacking.get().getShipmentId());
            } else {
                processNewPackingInV1(packingRequestV2, packing);
            }
            packing = packingDao.save(packing);
            PackingResponse response = objectMapper.convertValue(packing, PackingResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception ex) {
            String responseMsg = ex.getMessage() != null ? ex.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, ex);
            throw new RuntimeException(ex);
        }
    }

    private void processNewPackingInV1(PackingRequestV2 packingRequestV2, Packing packing) {
        if (packingRequestV2.getShipmentGuid() != null) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(packingRequestV2.getShipmentGuid());
            if (shipmentDetails.isPresent())
                packing.setShipmentId(shipmentDetails.get().getId());
        }
        if (packingRequestV2.getConsolidationGuid() != null) {
            Optional<ConsolidationDetails> consolidationDetails = consolidationDao.findByGuid(packingRequestV2.getConsolidationGuid());
            if (consolidationDetails.isPresent())
                packing.setConsolidationId(consolidationDetails.get().getId());
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> V1BulkPackingCreateAndUpdate(CommonRequestModel commonRequestModel) {
        BulkPackingRequestV2 bulkContainerRequest = (BulkPackingRequestV2) commonRequestModel.getData();
        try {
            List<ResponseEntity<?>> responses = new ArrayList<>();
            for (PackingRequestV2 containerRequest : bulkContainerRequest.getBulkPacking())
                responses.add(this.V1PackingCreateAndUpdate(CommonRequestModel.builder()
                        .data(containerRequest)
                        .build(), true));
            return ResponseHelper.buildSuccessResponse(responses);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            throw new RuntimeException(e);
        }
    }

    private IRunnerResponse convertEntityToDto(Packing packing) {
        return jsonHelper.convertValue(packing, PackingResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<Packing> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(packing -> {
            responseList.add(convertEntityToDto(packing));
        });
        return responseList;
    }


    /**
     * @param request
     * @return PackSummaryResponse
     * Api for giving out utilisation calculation of a given consolidation whenever packs are updated from the shipment
     * This will help consumer to identify the percentage of utilisation and act accordingly
     * This is just a calculation api , nothing's saved back into DB
     */
    // For inter branch context (hub/coload both possible) we are relying on source functions
    public PackSummaryResponse calculatePacksUtilisationForConsolidation(CalculatePackUtilizationRequest request) throws RunnerException {
        var consolidationId = request.getConsolidationId();
        var shipmentRequest = request.getShipmentRequest();
        var updatedConsolPacks = jsonHelper.convertValueToList(request.getPackingList(), Packing.class);
        var allocated = jsonHelper.convertValue(request.getAllocationsRequest(), Allocations.class);
        var attachingShipments = request.getShipmentIdList();

        Optional<ConsolidationDetails> optionalConsol = consolidationDao.findById(consolidationId);
        ConsolidationDetails consol = null;
        PackSummaryResponse packSummaryResponse = null;
        AchievedQuantities achievedQuantities = null;
        String toWeightUnit = Optional.ofNullable(request.getAllocationsRequest()).map(AllocationsRequest::getWeightUnit).orElse(Constants.WEIGHT_UNIT_KG);
        String toVolumeUnit = Optional.ofNullable(request.getAllocationsRequest()).map(AllocationsRequest::getVolumeUnit).orElse(Constants.VOLUME_UNIT_M3);

        var packingList = new ArrayList<Packing>();

        if(!optionalConsol.isPresent()) {
            return null;
        }
        if(shipmentRequest != null && (shipmentRequest.getId() == null))
            return null;

        consol = optionalConsol.get();
        if(!Objects.isNull(allocated)) {
            consol.setAllocations(allocated);
        }
        achievedQuantities = Optional.ofNullable(consol.getAchievedQuantities()).orElse(new AchievedQuantities());
        achievedQuantities.setConsolidatedWeightUnit(toWeightUnit);
        achievedQuantities.setConsolidatedVolumeUnit(toVolumeUnit);

        List<Packing> consolPackingList = Optional.ofNullable(consol.getPackingList()).orElse(Collections.emptyList());

        if(shipmentRequest != null) {
            // Filter out the old shipment-linked packs from the consol packs stream
            packingList.addAll(consolPackingList.stream().filter(i -> !Objects.equals(i.getShipmentId(),shipmentRequest.getId())).toList());
            // Add the current updated packs of the shipment
            var shipmentPackingList = jsonHelper.convertValueToList(shipmentRequest.getPackingList(), Packing.class);
            packingList.addAll(Optional.ofNullable(shipmentPackingList).orElse(Collections.emptyList()));
        }
        else if (attachingShipments != null && !attachingShipments.isEmpty()) {
            Set<Long> packingIdSet = new HashSet<>();
            if(!Boolean.TRUE.equals(request.getIgnoreConsolidationPacks())) {
                packingList.addAll(consolPackingList);
                packingIdSet = consolPackingList.stream().map(Packing::getId).collect(Collectors.toSet());
            }
            Set<Long> finalPackingIdSet = packingIdSet;
            packingList.addAll(getShipmentPacks(attachingShipments).stream().filter(i -> !finalPackingIdSet.contains(i.getId())).toList());
        }
        else {
            // Default case of packs updated from consol
            packingList.addAll(Optional.ofNullable(updatedConsolPacks).orElse(Optional.ofNullable(consol.getPackingList()).orElse(Collections.emptyList())));
        }

        log.info("calculating pack summary for aggregate of {} packs", packingList.size());
        packSummaryResponse = calculatePackSummary(packingList, TRANSPORT_MODE_AIR, null, new ShipmentMeasurementDetailsDto());
        log.info("Received weight: {} and volume:{} from packing summary response", packSummaryResponse.getAchievedWeight(), packSummaryResponse.getAchievedVolume());

        // only process the below calculation if the consolidation is air , exp , co-loading = true, allocated != null
        if(coLoadingConsolChecks(consol)) {
            var convertedWeight = BigDecimal.valueOf(convertUnit(MASS, packSummaryResponse.getAchievedWeight(), packSummaryResponse.getWeightUnit(), toWeightUnit).doubleValue());
            var convertedVolume = BigDecimal.valueOf(convertUnit(VOLUME, packSummaryResponse.getAchievedVolume(), packSummaryResponse.getVolumeUnit(), toVolumeUnit).doubleValue());
            achievedQuantities.setConsolidatedWeight(convertedWeight);
            achievedQuantities.setConsolidatedVolume(convertedVolume);
            consol.setAchievedQuantities(achievedQuantities);
            consol = commonUtils.calculateConsolUtilization(consol);
            packSummaryResponse.setConsolidationAchievedQuantities(jsonHelper.convertValue(consol.getAchievedQuantities(), AchievedQuantitiesResponse.class));
            packSummaryResponse.setAllocatedWeight(consol.getAllocations().getWeight());
            packSummaryResponse.setAllocatedVolume(consol.getAllocations().getVolume());
            packSummaryResponse.setAllocationsResponse(jsonHelper.convertValue(consol.getAllocations(), AllocationsResponse.class));
        }

        return packSummaryResponse;
    }

    private boolean coLoadingConsolChecks(ConsolidationDetails consol) {
        boolean flag = true;
        if(!consol.getTransportMode().equalsIgnoreCase(TRANSPORT_MODE_AIR))
            flag = false;
        if(!Boolean.TRUE.equals(commonUtils.getCurrentTenantSettings().getIsMAWBColoadingEnabled()))
            flag = false;
        if(!(consol.getAllocations() != null && consol.getAllocations().getWeight() != null))
            flag = false;
        return flag;
    }

    /**
     * @param shipmentIds
     * @return packingList of shipments that are TO-BE attached
     */
    private List<Packing> getShipmentPacks(List<Long> shipmentIds) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("id", shipmentIds, "IN");
        Pair<Specification<ShipmentDetails>, Pageable> pair = fetchData(listCommonRequest, ShipmentDetails.class);
        Page<ShipmentDetails> shipmentsPage = shipmentDao.findAll(pair.getLeft(), pair.getRight());

        return shipmentsPage.getContent().stream().map(ShipmentDetails::getPackingList).flatMap(Collection::stream).toList();
    }

    @Override
    @Transactional
    public void savePackUtilisationCalculationInConsole(CalculatePackUtilizationRequest calculatePackUtilizationRequest) {
        try {
            Optional<ConsolidationDetails> optional = consolidationDao.findById(calculatePackUtilizationRequest.getConsolidationId());
            if(optional.isPresent() && Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(optional.get().getTransportMode())) {
                var consol = optional.get();
                calculatePacksUtilisationForConsolidation(calculatePackUtilizationRequest);
                consolidationDao.save(consol, false);
            }
        }
        catch (Exception e) {
            log.error("Error saving pack utilisation in console : {}", e.getMessage());
        }

    }

}
