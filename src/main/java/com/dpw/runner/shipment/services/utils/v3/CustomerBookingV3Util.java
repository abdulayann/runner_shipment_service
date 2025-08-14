package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.repository.interfaces.ICustomerBookingRepository;
import com.dpw.runner.shipment.services.service.impl.ConsolidationV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.UnitConversionUtility.convertUnit;

@Component
@Slf4j
public class CustomerBookingV3Util {
    @Autowired
    private IContainerDao containerDao;
    @Autowired
    private IPackingDao packingDao;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private ICustomerBookingDao customerBookingDao;
    @Autowired
    private ICustomerBookingRepository customerBookingRepository;
    @Autowired
    private ConsolidationV3Service consolidationService;

    public void updateCargoInformation(CustomerBooking booking, Map<String, BigDecimal> codeTeuMap, CustomerBooking oldBooking, boolean isForMigration) throws RunnerException {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        List<Containers> containers = new ArrayList<>();
        List<Packing> packings = new ArrayList<>();
        if(booking.getId() != null) {
            containers = containerDao.findByBookingIdIn(List.of(booking.getId()));
            packings = packingDao.findByBookingIdIn(List.of(booking.getId()));
        }
        if (containers.isEmpty() && packings.isEmpty()) {
            saveBooking(booking, isForMigration);
            return;
        }
        if(containers.isEmpty()) {
            booking.setContainers(null);
            booking.setTeuCount(null);
        }
        else {
            updateContainersAndTeuInBooking(containers, codeTeuMap, booking);
        }
        updatePackagesInBookingCargoSummary(containers, packings, booking);
        updateWeightInBookingCargoSummary(containers, packings, booking, shipmentSettingsDetails);
        updateVolumeInBookingCargoSummary(packings, booking);
        calculateVW(booking, oldBooking);
        saveBooking(booking, isForMigration);
    }

    private void saveBooking(CustomerBooking booking, boolean isForMigration) {
        if (isForMigration) {
            customerBookingRepository.save(booking);
        } else {
            customerBookingDao.save(booking);
        }
    }

    public void updateVolumeInBookingCargoSummary(List<Packing> packingList, CustomerBooking booking) throws RunnerException {
        BigDecimal totalVolume = BigDecimal.ZERO;
        List<String> volumeUnits = packingList.stream()
                .map(Packing::getVolumeUnit)
                .filter(Objects::nonNull)
                .toList();
        String volumeUnit = resolveUnit(volumeUnits, VOLUME_UNIT_M3);
        booking.setVolumeUnit(volumeUnit);
        if(!packingList.isEmpty()) {
            for (Packing packing : packingList) {
                totalVolume = addVolume(packing, totalVolume, volumeUnit);
            }
            booking.setVolume(totalVolume);
        }
    }

    public void updatePackagesInBookingCargoSummary(List<Containers> containersList, List<Packing> packingList, CustomerBooking booking) {
        List<String> containerPackUnits = containersList.stream()
                .map(Containers::getContainerPackageType)
                .filter(Objects::nonNull)
                .toList();
        String containerPackType = resolveUnit(containerPackUnits, PackingConstants.PKG);

        List<String> packagePackUnits = packingList.stream()
                .map(Packing::getPacksType)
                .filter(Objects::nonNull)
                .toList();
        String packagePackType = resolveUnit(packagePackUnits, PackingConstants.PKG);

        if(!packingList.isEmpty()) {
            Long totalPacks = 0L;
            for(Packing packing: packingList) {
                if (!isStringNullOrEmpty(packing.getPacks())) {
                    totalPacks += Long.parseLong(packing.getPacks());
                }
            }
            booking.setPackages(totalPacks);
            booking.setPackageType(packagePackType);
        } else if (!containersList.isEmpty()){
            Long totalPacks = getTotalContainerPackages(containersList);
            booking.setPackages(totalPacks);
            booking.setPackageType(containerPackType);
        }
        booking.setPackingList(packingList);
    }

    public void updateWeightInBookingCargoSummary(List<Containers> containersList, List<Packing> packingList, CustomerBooking booking, ShipmentSettingsDetails shipmentSettingsDetails) throws RunnerException {
        String defaultBranchWeightUnit = consolidationService.determineWeightChargeableUnit(shipmentSettingsDetails);
        String containerWeightUnit = resolveUnit(
                containersList.stream()
                        .map(Containers::getContainerWeightUnit)
                        .filter(Objects::nonNull)
                        .toList(),
                defaultBranchWeightUnit
        );

        String packingWeightUnit = resolveUnit(
                packingList.stream()
                        .map(Packing::getPackWeightUnit)
                        .filter(Objects::nonNull)
                        .toList(),
                defaultBranchWeightUnit
        );
        booking.setGrossWeightUnit(defaultBranchWeightUnit);
        if (!packingList.isEmpty()) {
            boolean ifAnyPackMissedWeight = false;
            for (Packing pack : packingList) {
                if (pack.getCargoWeightPerPack() == null) {
                    ifAnyPackMissedWeight = true;
                    break;
                }
            }

            if (!ifAnyPackMissedWeight) {
                booking.setGrossWeight(getTotalCargoWeightFromPackages(packingList, packingWeightUnit));
                booking.setGrossWeightUnit(packingWeightUnit);
            } else if(!containersList.isEmpty()) {
                booking.setGrossWeight(getTotalCargoWeight(containersList, containerWeightUnit));
                booking.setGrossWeightUnit(containerWeightUnit);
            }
        } else if(!containersList.isEmpty()) {
            booking.setGrossWeight(getTotalCargoWeight(containersList, containerWeightUnit));
            booking.setGrossWeightUnit(containerWeightUnit);
        }
    }

    public void updateContainersAndTeuInBooking(List<Containers> containersList, Map<String, BigDecimal> codeTeuMap, CustomerBooking booking) {
        for(Containers containers: containersList) {
            containers.setTeu(codeTeuMap.get(containers.getContainerCode()));
            Long containerCount = containers.getContainerCount();
            BigDecimal weightPerContainer = containers.getCargoWeightPerContainer();
            BigDecimal containerWeight = BigDecimal.valueOf(containerCount != null ? containerCount : 0).multiply(weightPerContainer != null ? weightPerContainer : BigDecimal.ZERO);
            if(Objects.isNull(containers.getGrossWeight())) {
                containers.setGrossWeight(containerWeight);
            }
            containers.setGrossWeightUnit(containers.getContainerWeightUnit());
            if(!Objects.isNull(containers.getPackagesPerContainer())) {
                Long packsPerContainer = containers.getPackagesPerContainer();
                containers.setPacks(String.valueOf(BigDecimal.valueOf(containerCount != null ? containerCount : 0).multiply(new BigDecimal(packsPerContainer))));
            }
            if(!Objects.isNull(containers.getContainerPackageType())) {
                containers.setPacksType(containers.getContainerPackageType());
            }
        }
        BigDecimal teuCount = containersList.stream()
                .map(c -> codeTeuMap.getOrDefault(c.getContainerCode(), BigDecimal.ZERO)
                        .multiply(BigDecimal.valueOf(Optional.ofNullable(c.getContainerCount()).orElse(0L))))
                .reduce(BigDecimal.ZERO, BigDecimal::add)
                .setScale(1, RoundingMode.HALF_UP);
        booking.setTeuCount(teuCount);
        booking.setContainers(getTotalContainerCount(containersList));
        booking.setContainersList(containersList);
    }

    public BigDecimal getTotalCargoWeightFromPackages(List<Packing> packingList, String weightUnit) throws RunnerException {
        BigDecimal totalCargoWeight = BigDecimal.ZERO;
        for (Packing packing : packingList) {
            BigDecimal packingCount = packing.getPacks() != null ? new BigDecimal(packing.getPacks()) : BigDecimal.ZERO;
            BigDecimal weightPerPack = packing.getCargoWeightPerPack() != null ? packing.getCargoWeightPerPack() : BigDecimal.ZERO;
            BigDecimal totalLineCargoWeight = new BigDecimal(convertUnit(MASS, packingCount.multiply(weightPerPack), packing.getPackWeightUnit(), weightUnit).toString());

            totalCargoWeight = totalCargoWeight.add(totalLineCargoWeight);
        }
        return totalCargoWeight;
    }

    private Long getTotalContainerCount(List<Containers> containers) {
        return containers.stream().mapToLong(c -> c.getContainerCount() != null ? c.getContainerCount() : 0).sum();
    }

    public BigDecimal getTotalCargoWeight(List<Containers> containersList, String weightUnit) throws RunnerException {
        BigDecimal totalCargoWeight = BigDecimal.ZERO;
        for (Containers container : containersList) {
            BigDecimal containerCount = container.getContainerCount() != null ? new BigDecimal(container.getContainerCount()) : BigDecimal.ZERO;
            BigDecimal weightPerContainer = container.getCargoWeightPerContainer() != null ? container.getCargoWeightPerContainer() : BigDecimal.ZERO;
            BigDecimal totalLineCargoWeight = new BigDecimal(convertUnit(MASS, containerCount.multiply(weightPerContainer), container.getGrossWeightUnit(), weightUnit).toString());

            totalCargoWeight = totalCargoWeight.add(totalLineCargoWeight);
        }
        return totalCargoWeight;
    }

    public Long getTotalContainerPackages(List<Containers> containersList) {
        long totalLinePackages;
        long totalCargoSummaryPackages = 0L;
        for(Containers container: containersList) {
            long containerCount = container.getContainerCount() != null ? container.getContainerCount() : 0L;
            long packagesPerContainer = container.getPackagesPerContainer() != null ? container.getPackagesPerContainer() : 0L;
            totalLinePackages = containerCount * packagesPerContainer;

            totalCargoSummaryPackages += totalLinePackages;
        }
        return totalCargoSummaryPackages;
    }

    public String resolveUnit(List<String> unitsFromData, String branchDefaultUnit) {
        Set<String> distinctUnits = unitsFromData.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());

        return (distinctUnits.size() == 1) ? distinctUnits.iterator().next() : branchDefaultUnit;
    }

    private BigDecimal addVolume(Packing p, BigDecimal totalVolume, String volumeUnit) throws RunnerException {
        if (p.getVolume() != null && !isStringNullOrEmpty(p.getVolumeUnit())) {
            BigDecimal converted = new BigDecimal(convertUnit(VOLUME, p.getVolume(), p.getVolumeUnit(), volumeUnit).toString());
            return totalVolume.add(converted);
        }
        return totalVolume;
    }

    public void calculateVW(CustomerBooking customerBooking, CustomerBooking oldCustomerBooking) throws RunnerException {
        if (isStringNullOrEmpty(customerBooking.getTransportType()))
            return;
        boolean weightOrVolumeUpdated = isWeightOrVolumeUpdated(customerBooking, oldCustomerBooking);
        if (!isStringNullOrEmpty(customerBooking.getGrossWeightUnit()) && !isStringNullOrEmpty(customerBooking.getVolumeUnit())) {
            VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(customerBooking.getTransportType(), customerBooking.getGrossWeightUnit(), customerBooking.getVolumeUnit(), customerBooking.getGrossWeight(), customerBooking.getVolume());
            if (weightOrVolumeUpdated) {
                BigDecimal chargeable = vwOb.getChargeable();
                if (Constants.TRANSPORT_MODE_AIR.equals(customerBooking.getTransportType())) {
                    chargeable = BigDecimal.valueOf(roundOffAirShipment(chargeable.doubleValue()));
                }
                // LCL Sea transport special case
                if (Constants.TRANSPORT_MODE_SEA.equals(customerBooking.getTransportType()) && !isStringNullOrEmpty(customerBooking.getCargoType()) && Constants.SHIPMENT_TYPE_LCL.equals(customerBooking.getCargoType())) {
                    double volInM3 = convertUnit(Constants.VOLUME, customerBooking.getVolume(), customerBooking.getVolumeUnit(), Constants.VOLUME_UNIT_M3).doubleValue();
                    double wtInKg = convertUnit(Constants.MASS, customerBooking.getGrossWeight(), customerBooking.getGrossWeightUnit(), Constants.WEIGHT_UNIT_KG).doubleValue();
                    chargeable = BigDecimal.valueOf(Math.max(wtInKg / 1000, volInM3));
                    customerBooking.setChargeableUnit(Constants.VOLUME_UNIT_M3);
                    vwOb = consolidationService.calculateVolumeWeight(customerBooking.getTransportType(), Constants.WEIGHT_UNIT_KG, Constants.VOLUME_UNIT_M3, BigDecimal.valueOf(wtInKg), BigDecimal.valueOf(volInM3));
                } else {
                    customerBooking.setChargeableUnit(vwOb.getChargeableUnit());
                }
                customerBooking.setChargeable(chargeable);
            }
            customerBooking.setWeightVolume(vwOb.getVolumeWeight());
            customerBooking.setWeightVolumeUnit(vwOb.getVolumeWeightUnit());
        }
    }

    private boolean isWeightOrVolumeUpdated(CustomerBooking newBooking, CustomerBooking oldBooking) {
        if (oldBooking == null) {
            return true;
        }
        return !newBooking.getGrossWeight().equals(oldBooking.getGrossWeight()) || !newBooking.getVolume().equals(oldBooking.getVolume());
    }

    private double roundOffAirShipment(double charge) {
        return (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) ?
                Math.floor(charge) + 0.5 : Math.ceil(charge);
    }
}
