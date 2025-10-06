package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShippingInstructionContainerWarningResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingBridgeRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionInttraRequest;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.MasterDataHelper;
import com.dpw.runner.shipment.services.projection.ShippingConsoleIdProjection;
import com.dpw.runner.shipment.services.projection.ShippingConsoleNoProjection;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Slf4j
@Component
public class ShippingInstructionUtil {

    @Autowired
    private ICommonContainersDao commonContainersDao;

    @Autowired
    private ICommonPackagesDao commonPackagesDao;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShippingInstructionDao shippingInstructionDao;

    @Autowired
    private IContainerDao containerDao;

    public List<ShippingInstructionContainerWarningResponse> compareContainerDetails(
            List<CommonContainers> oldContainers,
            List<CommonContainers> newContainers) {

        List<ShippingInstructionContainerWarningResponse> warnings = new ArrayList<>();

        try {
            if (oldContainers == null) oldContainers = Collections.emptyList();
            if (newContainers == null) newContainers = Collections.emptyList();

            Map<Long, CommonContainers> oldMap = oldContainers.stream()
                    .collect(Collectors.toMap(CommonContainers::getId, Function.identity(), (a, b) -> a));

            for (CommonContainers newContainer : newContainers) {
                CommonContainers oldContainer = oldMap.get(newContainer.getId());
                if (oldContainer != null) {

                    boolean packChanged = !Objects.equals(oldContainer.getPacks(), newContainer.getPacks())
                            || !Objects.equals(oldContainer.getPacksUnit(), newContainer.getPacksUnit());

                    boolean weightChanged = !bigDecimalEquals(oldContainer.getGrossWeight(), newContainer.getGrossWeight())
                            || !Objects.equals(oldContainer.getGrossWeightUnit(), newContainer.getGrossWeightUnit());

                    if (packChanged || weightChanged) {
                        ShippingInstructionContainerWarningResponse resp = buildContainerWarning(newContainer, oldContainer);
                        warnings.add(resp);
                    }
                }
            }

            return warnings;
        } catch (Exception e) {
            log.error("Exception during comparing container details {}", e.getMessage());
        }

        return warnings;
    }

    private static boolean bigDecimalEquals(BigDecimal a, BigDecimal b) {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;
        return a.compareTo(b) == 0;
    }

    @NotNull
    private static ShippingInstructionContainerWarningResponse buildContainerWarning(CommonContainers newContainer, CommonContainers oldContainer) {
        ShippingInstructionContainerWarningResponse resp = new ShippingInstructionContainerWarningResponse();
        resp.setContainerNumber(newContainer.getContainerNo());
        resp.setPackagePrev(formatPacks(oldContainer.getPacks(), oldContainer.getPacksUnit()));
        resp.setPackagePost(formatPacks(newContainer.getPacks(), newContainer.getPacksUnit()));
        resp.setWeightPrevious(formatWeight(oldContainer.getGrossWeight(), oldContainer.getGrossWeightUnit()));
        resp.setWeightPost(formatWeight(newContainer.getGrossWeight(), newContainer.getGrossWeightUnit()));
        return resp;
    }

    public List<ShippingInstructionContainerWarningResponse> comparePackageDetails(
            List<CommonPackages> oldPackages,
            List<CommonPackages> newPackages) {

        if (oldPackages == null) oldPackages = Collections.emptyList();
        if (newPackages == null) newPackages = Collections.emptyList();

        // Keep mapping by containerNo (as your current logic uses containerNo for packages)
        Map<Long, CommonPackages> oldMap = oldPackages.stream()
                .collect(Collectors.toMap(CommonPackages::getId, Function.identity(), (a, b) -> a));

        List<ShippingInstructionContainerWarningResponse> warnings = new ArrayList<>();

        for (CommonPackages newPackage : newPackages) {
            CommonPackages oldPackage = oldMap.get(newPackage.getId());
            if (oldPackage != null) {
                boolean packChanged = !Objects.equals(oldPackage.getPacks(), newPackage.getPacks())
                        || !Objects.equals(oldPackage.getPacksUnit(), newPackage.getPacksUnit());

                boolean weightChanged = !bigDecimalEquals(oldPackage.getGrossWeight(), newPackage.getGrossWeight())
                        || !Objects.equals(oldPackage.getGrossWeightUnit(), newPackage.getGrossWeightUnit());

                if (packChanged || weightChanged) {
                    ShippingInstructionContainerWarningResponse resp = buildPackageWarning(newPackage, oldPackage);
                    warnings.add(resp);
                }
            }
        }

        return warnings;
    }

    @NotNull
    private static ShippingInstructionContainerWarningResponse buildPackageWarning(CommonPackages newPackage, CommonPackages oldPackage) {
        ShippingInstructionContainerWarningResponse resp = new ShippingInstructionContainerWarningResponse();
        resp.setContainerNumber(newPackage.getContainerNo());
        resp.setPackagePrev(formatPacks(oldPackage.getPacks(), oldPackage.getPacksUnit()));
        resp.setPackagePost(formatPacks(newPackage.getPacks(), newPackage.getPacksUnit()));
        resp.setWeightPrevious(formatWeight(oldPackage.getGrossWeight(), oldPackage.getGrossWeightUnit()));
        resp.setWeightPost(formatWeight(newPackage.getGrossWeight(), newPackage.getGrossWeightUnit()));
        return resp;
    }

    private static String formatPacks(Integer packs, String unit) {
        if (packs == null) packs = 0;
        if (unit == null || unit.isBlank()) return String.valueOf(packs);
        return packs + " " + unit;
    }

    private static String formatWeight(BigDecimal weight, String unit) {
        if (weight == null) weight = BigDecimal.valueOf(0);
        String w = weight.stripTrailingZeros().toPlainString();
        if (unit == null || unit.isBlank()) return w;
        return w + " " + unit;
    }


    public void syncCommonContainersByConsolId(Long consolId) {
        if (consolId == null) {
            return;
        }

        // --- Step 1: Resolve SI id(s) for this consolidation ---
        List<Long> siIds = new ArrayList<>();

        // 1a: Direct lookup (entityType = CONSOLIDATION)
        List<ShippingConsoleIdProjection> direct =
                shippingInstructionDao.findByEntityTypeAndEntityIdIn(EntityType.CONSOLIDATION, List.of(consolId));

        if (!direct.isEmpty()) {
            siIds.addAll(direct.stream()
                    .map(ShippingConsoleIdProjection::getId)
                    .filter(Objects::nonNull)
                    .toList());
        }

        // 1b: If nothing found, try via CarrierBooking
        if (siIds.isEmpty()) {
            List<ShippingConsoleIdProjection> viaCarrier =
                    shippingInstructionDao.findByCarrierBookingConsolId(List.of(consolId));

            siIds.addAll(viaCarrier.stream()
                    .map(ShippingConsoleIdProjection::getId)
                    .filter(Objects::nonNull)
                    .toList());
        }

        if (siIds.isEmpty()) {
            // no SI mapped, nothing to sync
            return;
        }

        // --- Step 2: Fetch all containers for this consolidation ---
        List<Containers> containers = containerDao.findByConsolidationId(consolId);
        if (containers == null || containers.isEmpty()) {
            return;
        }

        // --- Step 3: Fetch existing common_containers for all these guids ---
        List<UUID> guids = containers.stream()
                .map(Containers::getGuid)
                .filter(Objects::nonNull)
                .toList();

        Map<UUID, CommonContainers> commonMap = new HashMap<>();
        Set<UUID> conflictedGuids = new HashSet<>();

        for (CommonContainers cc : commonContainersDao.getAll(guids)) {
            UUID guid = cc.getContainerRefGuid();
            if (guid == null) continue;

            if (commonMap.containsKey(guid)) {
                conflictedGuids.add(guid);
                log.warn("Duplicate containerRefGuid found: {}. Skipping both entries.", guid);
            } else if (!conflictedGuids.contains(guid)) {
                commonMap.put(guid, cc);
            }
        }

        List<CommonContainers> toSave = new ArrayList<>();

        // --- Step 4: Update each container’s common record ---
        for (Containers container : containers) {
            if (container.getGuid() == null) {
                continue; // skip invalid container
            }

            CommonContainers common = commonMap.get(container.getGuid());
            if (common == null) {
                common = new CommonContainers();
                common.setContainerRefGuid(container.getGuid());
            }

            // copy details from container -> common
            updateCommonContainerFromContainer(common, container);

            // attach first SI id (if multiple exist)
            if (!siIds.isEmpty()) {
                common.setShippingInstructionId(siIds.get(0));
            }

            toSave.add(common);
        }

        // --- Step 5: Save all updated commons ---
        if (!toSave.isEmpty()) {
            commonContainersDao.saveAll(toSave);
        }
    }




    public void syncCommonPackings(List<Packing> packings) {
        if (packings == null || packings.isEmpty()) {
            return;
        }

        // --- Step 1: Fetch shipment details in bulk ---
        List<Long> shipmentIds = packings.stream()
                .map(Packing::getShipmentId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        if (shipmentIds.isEmpty()) {
            return;
        }

        List<ShipmentDetails> shipmentDetailsList = shipmentDao.findByIdIn(shipmentIds);

        // Build shipmentId → consolidationNumbers map
        Map<Long, List<String>> shipmentToConsolMap = shipmentDetailsList.stream()
                .collect(Collectors.toMap(
                        ShipmentDetails::getId,
                        sd -> sd.getConsolidationList() == null ? List.of() :
                                sd.getConsolidationList().stream()
                                        .map(ConsolidationDetails::getConsolidationNumber)
                                        .filter(Objects::nonNull)
                                        .toList()
                ));

        // --- Step 2: Collect all unique consol numbers ---
        List<String> allConsolNumbers = shipmentToConsolMap.values().stream()
                .flatMap(List::stream)
                .filter(Objects::nonNull)
                .distinct()
                .toList();

        if (allConsolNumbers.isEmpty()) {
            return;
        }

        List<Long> siIds = new ArrayList<>();
        // --- Step 2a: Direct lookup (entityType = CONSOLIDATION) ---
        List<ShippingConsoleNoProjection> direct = shippingInstructionDao
                .findByEntityTypeAndEntityNoIn(EntityType.CONSOLIDATION, allConsolNumbers);

        if (!direct.isEmpty()) {
            siIds.addAll(direct.stream()
                    .map(ShippingConsoleNoProjection::getId)
                    .filter(Objects::nonNull)
                    .toList());
        }

        Map<String, List<Long>> consolToInstructionMap = direct.stream()
                .filter(p -> p.getEntityId() != null)
                .collect(Collectors.groupingBy(
                        ShippingConsoleNoProjection::getEntityId,
                        Collectors.mapping(ShippingConsoleNoProjection::getId, Collectors.toList())
                ));

// --- Step 2b: Indirect lookup via CarrierBooking for missing consol numbers ---
        List<String> missingConsols = allConsolNumbers.stream()
                .filter(c -> !consolToInstructionMap.containsKey(c))
                .toList();

        if (!missingConsols.isEmpty()) {
            List<ShippingConsoleNoProjection> viaCarrier = shippingInstructionDao
                    .findByCarrierBookingConsolNumbers(missingConsols);

            viaCarrier.stream()
                    .filter(p -> p.getEntityId() != null)
                    .forEach(p -> consolToInstructionMap
                            .computeIfAbsent(p.getEntityId(), k -> new ArrayList<>())
                            .add(p.getId()));
            siIds.addAll(viaCarrier.stream()
                    .map(ShippingConsoleNoProjection::getId)
                    .filter(Objects::nonNull)
                    .toList());
        }

        // --- Step 3: Fetch existing commons ---
        List<UUID> guids = packings.stream()
                .map(Packing::getGuid)
                .filter(Objects::nonNull)
                .toList();

        if (guids.isEmpty()) {
            return;
        }

        Map<UUID, CommonPackages> commonMap = commonPackagesDao.findByPackingRefGuidIn(guids)
                .stream()
                .collect(Collectors.toMap(CommonPackages::getPackingRefGuid, p -> p));

        List<CommonPackages> toSave = new ArrayList<>();

        for (Packing packing : packings) {
            CommonPackages common = commonMap.get(packing.getGuid());
            if (common != null) {
                updateCommonPackingFromPacking(common, packing, siIds.get(0));
                toSave.add(common);
            } else {
                CommonPackages newCommon = new CommonPackages();
                newCommon.setPackingRefGuid(packing.getGuid()); // ref guid
                updateCommonPackingFromPacking(newCommon, packing, siIds.get(0));
                toSave.add(newCommon);
            }
        }
        commonPackagesDao.saveAll(toSave);
    }

    public void updateCommonContainerFromContainer(CommonContainers common, Containers container) {
        common.setContainerCode(container.getContainerCode());
        common.setCount(container.getContainerCount());
        common.setGoodsDescription(container.getDescriptionOfGoods());
        common.setHsCode(container.getHsCode());
        common.setCommodityCode(container.getCommodityCode());
        common.setCommodityGroup(container.getCommodityGroup());
        common.setMarksNums(container.getMarksNums());
        common.setGrossWeight(container.getGrossWeight());
        common.setVolume(container.getGrossVolume());
        common.setNetWeight(container.getNetWeight());
        common.setNetWeightUnit(container.getNetWeightUnit());
        common.setGrossWeightUnit(container.getGrossWeightUnit());
        common.setVolumeUnit(container.getGrossVolumeUnit());
        common.setContainerNo(container.getContainerNumber());
        common.setPacks(container.getPacks() != null ? Integer.valueOf(container.getPacks()) : null);
        common.setPacksUnit(container.getPacksType());
        common.setTareWeight(container.getTareWeight());
        common.setTareWeightUnit(container.getTareWeightUnit());
        common.setSealNumber(container.getSealNumber());
        common.setShipperSealNumber(container.getShipperSealNumber());
        common.setVeterinarySealNumber(container.getVeterinarySealNumber());
        common.setCustomsSealNumber(container.getCustomsSealNumber());
    }

    public void updateCommonPackingFromPacking(CommonPackages common, Packing packing, Long siId) {
        common.setContainerNo(packing.getContainerId() != null ? String.valueOf(packing.getContainerId()) : null);
        common.setPacks(tryParseInt(packing.getPacks(), null));
        common.setPacksUnit(packing.getPacksType());
        common.setHsCode(packing.getHSCode());
        common.setCommodityCode(packing.getCommodity());
        common.setCommodityGroup(packing.getCommodityGroup());
        common.setMarksnNums(packing.getMarksnNums());
        common.setGoodsDescription(packing.getGoodsDescription());
        common.setGrossWeight(packing.getWeight());
        common.setGrossWeightUnit(packing.getWeightUnit());
        common.setVolume(packing.getVolume());
        common.setVolumeUnit(packing.getVolumeUnit());
        common.setShippingInstructionId(siId);
    }


    private Integer tryParseInt(String value, Integer fallback) {
        try {
            return value != null ? Integer.valueOf(value) : fallback;
        } catch (NumberFormatException e) {
            return fallback;
        }
    }

    public void populateInttraSpecificData(ShippingInstructionInttraRequest instructionInttraResponse, String inttraId) throws RunnerException {
        List<CommonContainerResponse> containers = instructionInttraResponse.getCommonContainersList();

        if (containers == null) {
            setDefaultTotals(instructionInttraResponse);
            instructionInttraResponse.setInttraOrgId(inttraId);
            return;
        }

        calculateEquipmentTotals(instructionInttraResponse, containers);
        convertUnitsAndCalculateTotals(instructionInttraResponse, containers);
        instructionInttraResponse.setInttraOrgId(inttraId);
    }

    private void setDefaultTotals(ShippingInstructionInttraRequest response) {
        response.setTotalNumberOfEquipments(0);
        response.setTotalNoOfPackages(0);
        response.setTotalGrossWeight(0.0);
        response.setTotalGrossVolume(0.0);
    }

    private void calculateEquipmentTotals(ShippingInstructionInttraRequest response, List<CommonContainerResponse> containers) {
        int totalEquipments = containers.stream()
                .mapToInt(container -> container.getCount() != null ? container.getCount() : 1)
                .sum();

        int totalPackages = containers.stream()
                .mapToInt(container -> container.getPacks() != null ? container.getPacks() : 0)
                .sum();

        response.setTotalNumberOfEquipments(totalEquipments);
        response.setTotalNoOfPackages(totalPackages);
    }

    private void convertUnitsAndCalculateTotals(ShippingInstructionInttraRequest response, List<CommonContainerResponse> containers) throws RunnerException {
        double totalWeight = 0.0;
        double totalVolume = 0.0;

        for (CommonContainerResponse container : containers) {
            totalWeight += convertAndUpdateWeight(container);
            totalVolume += convertAndUpdateVolume(container);
        }

        response.setTotalGrossWeight(totalWeight);
        response.setTotalGrossVolume(totalVolume);
    }

    private double convertAndUpdateWeight(CommonContainerResponse container) throws RunnerException {
        double totalWeight = 0.0;

        // Convert and update gross weight
        if (container.getGrossWeight() != null) {
            BigDecimal convertedWeight = BigDecimal.valueOf(convertUnit(Constants.MASS, container.getGrossWeight(),
                    container.getGrossWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue());
            container.setGrossWeight(convertedWeight);
            container.setGrossWeightUnit(Constants.WEIGHT_UNIT_KG);
            totalWeight = convertedWeight.doubleValue();
        }

        // Convert and update net weight
        if (container.getNetWeight() != null) {
            BigDecimal convertedNetWeight = BigDecimal.valueOf(convertUnit(Constants.MASS, container.getNetWeight(),
                    container.getNetWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue());
            container.setNetWeight(convertedNetWeight);
            container.setNetWeightUnit(Constants.WEIGHT_UNIT_KG);
        }

        return totalWeight;
    }

    private double convertAndUpdateVolume(CommonContainerResponse container) throws RunnerException {
        if (container.getVolume() != null) {
            BigDecimal convertedVolume = BigDecimal.valueOf(convertUnit(Constants.VOLUME, container.getVolume(),
                    container.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue());
            container.setVolume(convertedVolume);
            container.setVolumeUnit(Constants.VOLUME_UNIT_M3);
            return convertedVolume.doubleValue();
        }
        return 0.0;
    }

    public void populateCarrierDetails(Map<String, EntityTransferCarrier> carrierDatav1Map, ShippingInstructionInttraRequest shippingInstructionInttraRequest) {

        if (Objects.isNull(carrierDatav1Map)) return;

        // Process each carrier and fetch the required details
        for (Map.Entry<String, EntityTransferCarrier> entry : carrierDatav1Map.entrySet()) {
            EntityTransferCarrier carrier = entry.getValue();

            String carrierScacCode = carrier.ItemValue;
            String carrierDescription = carrier.ItemDescription;

            // Set the fetched details in the VerifiedGrossMassInttraResponse
            shippingInstructionInttraRequest.setCarrierScacCode(carrierScacCode);
            shippingInstructionInttraRequest.setCarrierDescription(carrierDescription);
        }
    }

    public List<String> getSendEmailBaseRequest(ShippingInstruction shippingInstruction) {
        StringBuilder toEmails = new StringBuilder();

        // Add internal emails if present
        if (Objects.nonNull(shippingInstruction.getInternalEmails()) && !shippingInstruction.getInternalEmails().trim().isEmpty()) {
            toEmails.append(shippingInstruction.getInternalEmails());
        }

        // Add the 'createByUserEmail' only if it's not blank
        String createByUserEmail = shippingInstruction.getCreateByUserEmail();
        if (Objects.nonNull(createByUserEmail) && !createByUserEmail.trim().isEmpty()) {
            if (!toEmails.isEmpty()) {
                toEmails.append(",");
            }
            toEmails.append(createByUserEmail);
        }

        // Add the 'submitByUserEmail' only if it's not blank and different from 'createByUserEmail'
        String submitByUserEmail = shippingInstruction.getSubmitByUserEmail();
        if (Objects.nonNull(submitByUserEmail) && !submitByUserEmail.trim().isEmpty()
                && !submitByUserEmail.equalsIgnoreCase(createByUserEmail)) {
            if (!toEmails.isEmpty()) {
                toEmails.append(",");
            }
            toEmails.append(submitByUserEmail);
        }

        // Convert to list, trimming spaces and removing blanks
        return Arrays.stream(toEmails.toString().split(","))
                .map(String::trim)
                .filter(email -> !email.isEmpty())
                .distinct() // remove duplicates if any
                .toList();
    }
}
