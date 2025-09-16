package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShippingInstructionContainerWarningResponse;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.CommonPackages;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Component
public class ShipmentInstructionUtil {
    public List<ShippingInstructionContainerWarningResponse> compareContainerDetails(
            List<CommonContainers> oldContainers,
            List<CommonContainers> newContainers) {

        try {
            if (oldContainers == null) oldContainers = Collections.emptyList();
            if (newContainers == null) newContainers = Collections.emptyList();

            Map<String, CommonContainers> oldMap = oldContainers.stream()
                    .filter(c -> c.getContainerNo() != null)
                    .collect(Collectors.toMap(CommonContainers::getContainerNo, Function.identity(), (a, b) -> a));

            List<ShippingInstructionContainerWarningResponse> warnings = new ArrayList<>();

            for (CommonContainers newContainer : newContainers) {
                if (newContainer == null || newContainer.getContainerNo() == null) continue;

                CommonContainers oldContainer = oldMap.get(newContainer.getContainerNo());
                if (oldContainer == null) continue;

                boolean packChanged = !Objects.equals(oldContainer.getPacks(), newContainer.getPacks())
                        || !Objects.equals(oldContainer.getPacksUnit(), newContainer.getPacksUnit());

                boolean weightChanged = !bigDecimalEquals(oldContainer.getGrossWeight(), newContainer.getGrossWeight())
                        || !Objects.equals(oldContainer.getGrossWeightUnit(), newContainer.getGrossWeightUnit());

                if (packChanged || weightChanged) {
                    ShippingInstructionContainerWarningResponse resp = buildContainerWarning(newContainer, oldContainer);
                    warnings.add(resp);
                }
            }

            return warnings;
        } catch (Exception e){
            System.out.println(e.getMessage());
            return null;
        }
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
        Map<String, CommonPackages> oldMap = oldPackages.stream()
                .filter(p -> p.getContainerNo() != null)
                .collect(Collectors.toMap(CommonPackages::getContainerNo, Function.identity(), (a, b) -> a));

        List<ShippingInstructionContainerWarningResponse> warnings = new ArrayList<>();

        for (CommonPackages newPackage : newPackages) {
            if (newPackage == null || newPackage.getContainerNo() == null) continue;

            CommonPackages oldPackage = oldMap.get(newPackage.getContainerNo());
            if (oldPackage == null) continue;

            boolean packChanged = !Objects.equals(oldPackage.getPacks(), newPackage.getPacks())
                    || !Objects.equals(oldPackage.getPacksUnit(), newPackage.getPacksUnit());

            boolean weightChanged = !bigDecimalEquals(oldPackage.getGrossWeight(), newPackage.getGrossWeight())
                    || !Objects.equals(oldPackage.getGrossWeightUnit(), newPackage.getGrossWeightUnit());

            if (packChanged || weightChanged) {
                ShippingInstructionContainerWarningResponse resp = buildPackageWarning(newPackage, oldPackage);
                warnings.add(resp);
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
        if (packs == null) return null;
        if (unit == null || unit.isBlank()) return String.valueOf(packs);
        return packs + " " + unit;
    }

    private static String formatWeight(BigDecimal weight, String unit) {
        if (weight == null) return null;
        String w = weight.stripTrailingZeros().toPlainString();
        if (unit == null || unit.isBlank()) return w;
        return w + " " + unit;
    }
}
