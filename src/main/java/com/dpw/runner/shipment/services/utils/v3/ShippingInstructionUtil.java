package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dao.interfaces.ICommonContainersDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICommonPackagesDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShippingInstructionContainerWarningResponse;
import com.dpw.runner.shipment.services.entity.*;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Component
public class ShippingInstructionUtil {

    @Autowired
    private ICommonContainersDao commonContainersDao;

    @Autowired
    private ICommonPackagesDao commonPackagesDao;

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


    private void syncCommonContainers(List<Containers> containers) {
        if (containers == null || containers.isEmpty()) {
            return;
        }

        List<UUID> guids = containers.stream()
                .map(Containers::getGuid)
                .filter(Objects::nonNull)
                .toList();

        if (guids.isEmpty()) {
            return;
        }

        List<CommonContainers> commons = commonContainersDao.getAll(guids);

        Map<UUID, CommonContainers> commonMap = commons.stream()
                .collect(Collectors.toMap(CommonContainers::getContainerRefGuid, c -> c));

        List<CommonContainers> toSave = new ArrayList<>();

        for (Containers container : containers) {
            CommonContainers common = commonMap.get(container.getGuid());
            if (common != null) {
                updateCommonContainerFromContainer(common, container);
                toSave.add(common);
            } else {
                CommonContainers newCommon = new CommonContainers();
                newCommon.setContainerRefGuid(container.getGuid()); // ref guid
                updateCommonContainerFromContainer(newCommon, container);
                toSave.add(newCommon);
            }
        }

        commonContainersDao.saveAll(toSave);
    }

    private void syncCommonPackings(List<Packing> packings) {
        if (packings == null || packings.isEmpty()) {
            return;
        }

        List<UUID> guids = packings.stream()
                .map(Packing::getGuid)
                .filter(Objects::nonNull)
                .toList();

        if (guids.isEmpty()) {
            return;
        }

        List<CommonPackages> commons = commonPackagesDao.findByPackingRefGuidIn(guids);

        Map<UUID, CommonPackages> commonMap = commons.stream()
                .collect(Collectors.toMap(CommonPackages::getPackingRefGuid, p -> p));

        List<CommonPackages> toSave = new ArrayList<>();

        for (Packing packing : packings) {
            CommonPackages common = commonMap.get(packing.getGuid());
            if (common != null) {
                updateCommonPackingFromPacking(common, packing);
                toSave.add(common);
            } else {
                CommonPackages newCommon = new CommonPackages();
                newCommon.setPackingRefGuid(packing.getGuid()); // ref guid
                updateCommonPackingFromPacking(newCommon, packing);
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

    public void updateCommonPackingFromPacking(CommonPackages common, Packing packing) {
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
    }


    private Integer tryParseInt(String value, Integer fallback) {
        try {
            return value != null ? Integer.valueOf(value) : fallback;
        } catch (NumberFormatException e) {
            return fallback;
        }
    }

}
