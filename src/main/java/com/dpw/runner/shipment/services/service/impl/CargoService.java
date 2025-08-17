package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PackingConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CargoChargeableRequest;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoChargeableResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.dto.shipment_console_dtos.ShipmentSummaryWarningsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.v3.CustomerBookingV3Util;
import com.dpw.runner.shipment.services.utils.v3.ShipmentsV3Util;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.MASS;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;

@Service
@Slf4j
public class CargoService implements ICargoService {

    private final ICustomerBookingDao customerBookingDao;
    private final ConsolidationV3Service consolidationService;
    private final CustomerBookingV3Service customerBookingV3Service;
    private final CommonUtils commonUtils;
    private final ShipmentsV3Util shipmentsV3Util;
    private final CustomerBookingV3Util customerBookingV3Util;

    @Autowired
    public CargoService(ICustomerBookingDao customerBookingDao,
                        ConsolidationV3Service consolidationService,
                        CustomerBookingV3Service customerBookingV3Service,
                        CommonUtils commonUtils,
                        ShipmentsV3Util shipmentsV3Util,
                        CustomerBookingV3Util customerBookingV3Util) {
        this.customerBookingDao = customerBookingDao;
        this.consolidationService = consolidationService;
        this.customerBookingV3Service = customerBookingV3Service;
        this.commonUtils = commonUtils;
        this.shipmentsV3Util = shipmentsV3Util;
        this.customerBookingV3Util = customerBookingV3Util;
    }

    @Override
    public CargoDetailsResponse getCargoDetails(CargoDetailsRequest request) throws RunnerException {
        CargoDetailsResponse response = new CargoDetailsResponse();
        Long entityId = Long.valueOf(request.getEntityId());
        Optional<CustomerBooking> optionalCustomerBooking = customerBookingDao.findById(entityId);
        if(optionalCustomerBooking.isEmpty()) {
            throw new ValidationException("Booking not found with id "+entityId);
        }
        CustomerBooking customerBooking = optionalCustomerBooking.get();
        List<Containers> containers = customerBooking.getContainersList();
        List<Packing> packings = customerBooking.getPackingList();
        response.setTransportMode(customerBooking.getTransportType());
        response.setShipmentType(customerBooking.getCargoType());
        response.setContainers(customerBooking.getContainers() != null ? Math.toIntExact(customerBooking.getContainers()) : null);
        response.setTeuCount(customerBooking.getTeuCount());
        response.setNoOfPacks(customerBooking.getPackages() != null ? Math.toIntExact(customerBooking.getPackages()) : null);
        response.setPacksUnit(customerBooking.getPackageType());
        response.setWeight(customerBooking.getGrossWeight());
        response.setWeightUnit(customerBooking.getGrossWeightUnit());
        response.setVolume(customerBooking.getVolume());
        response.setVolumeUnit(customerBooking.getVolumeUnit());
        response.setVolumetricWeight(customerBooking.getWeightVolume());
        response.setVolumetricWeightUnit(customerBooking.getWeightVolumeUnit());
        response.setChargable(customerBooking.getChargeable());
        response.setChargeableUnit(customerBooking.getChargeableUnit());
        updateEditableFlags(response, containers, packings);
        updateSummaryWarnings(response, containers, packings);
        return response;
    }

    public void updateSummaryWarnings(CargoDetailsResponse response, List<Containers> containers, List<Packing> packings) throws RunnerException {
        //For Packings warnings
        ShipmentSummaryWarningsResponse.WarningDetail packageWarningDetails = getPackageSummaryWarning(containers, packings);
        //For Weight warnings
        ShipmentSummaryWarningsResponse.WarningDetail weightWarningDetails = getWeightSummaryWarning(containers, packings, response.getWeightUnit());
        response.setShipmentSummaryWarningsResponse(ShipmentSummaryWarningsResponse.builder().packagesWarning(packageWarningDetails).weightWarning(weightWarningDetails).build());
    }

    private ShipmentSummaryWarningsResponse.WarningDetail getWeightSummaryWarning(List<Containers> containers, List<Packing> packings, String bookingWeightUnit) throws RunnerException {
        ShipmentSummaryWarningsResponse.WarningDetail weightWarningDetails = null;
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        List<String> packingWeightUnits = packings.stream()
                .map(Packing::getWeightUnit)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        List<String> containerWeightUnits = containers.stream()
                .map(Containers::getContainerWeightUnit)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        String packsWeightUnit = customerBookingV3Util.resolveUnit(packingWeightUnits, consolidationService.determineWeightChargeableUnit(shipmentSettingsDetails));
        String containersWeightUnit = customerBookingV3Util.resolveUnit(containerWeightUnits, consolidationService.determineWeightChargeableUnit(shipmentSettingsDetails));

        BigDecimal containerWeight = customerBookingV3Util.getTotalCargoWeight(containers, containersWeightUnit);
        BigDecimal packageWeight = customerBookingV3Util.getTotalCargoWeightFromPackages(packings, packsWeightUnit);
        if (BigDecimal.ZERO.compareTo(packageWeight) == 0) {
            packageWeight = null;
        }
        if (BigDecimal.ZERO.compareTo(containerWeight) == 0) {
            containerWeight = null;
        }
        weightWarningDetails = shipmentsV3Util.generateWarning(
                packageWeight,
                packsWeightUnit,
                containerWeight,
                containersWeightUnit,
                MASS,
                bookingWeightUnit
        );
        return weightWarningDetails;
    }

    private ShipmentSummaryWarningsResponse.WarningDetail getPackageSummaryWarning(List<Containers> containers, List<Packing> packings) {
        ShipmentSummaryWarningsResponse.WarningDetail packageWarningDetails = null;
        Long totalContainerPackages = customerBookingV3Util.getTotalContainerPackages(containers);
        Set<String> containerPackageTypes = containers.stream().map(Containers::getContainerPackageType).collect(Collectors.toSet());
        Set<String> packageTypes = new HashSet<>();
        Long totalPackages = 0L;
        for(Packing packing: packings) {
            if (!isStringNullOrEmpty(packing.getPacks())) {
                totalPackages += Integer.parseInt(packing.getPacks());
            }
            if(!isStringNullOrEmpty(packing.getPacksType())) {
                packageTypes.add(packing.getPacksType());
            }
        }
        String containerPackType = (containerPackageTypes.size() == 1) ? containerPackageTypes.iterator().next() : PackingConstants.PKG;
        String packType = (packageTypes.size() == 1) ? packageTypes.iterator().next() : PackingConstants.PKG;
        if (!totalPackages.equals(0L) && !totalContainerPackages.equals(0L) && !totalPackages.equals(totalContainerPackages)) {
             packageWarningDetails = new ShipmentSummaryWarningsResponse.WarningDetail(
                    true,
                    totalContainerPackages + " " + containerPackType,
                    totalPackages + " " + packType,
                    Math.abs(totalContainerPackages - totalPackages) + " " + packType
            );
        }
        return packageWarningDetails;
    }

    public void updateEditableFlags(CargoDetailsResponse response, List<Containers> containers, List<Packing> packings) {
        if(packings.isEmpty() && containers.isEmpty()) {
            response.setIsCargoSummaryEditable(Boolean.TRUE);
        } else if(packings.isEmpty()) {
            response.setIsVolumeEditable(Boolean.TRUE);
        } else if(containers.isEmpty()) {
            boolean isWeightMissing = Boolean.FALSE;
            boolean isVolumeMissing = Boolean.FALSE;
            for(Packing packing : packings) {
                if(Objects.isNull(packing.getWeight())) {
                    isWeightMissing = Boolean.TRUE;
                    break;
                }
                if(Objects.isNull(packing.getVolume())) {
                    isVolumeMissing = Boolean.TRUE;
                    break;
                }
            }
            response.setIsVolumeEditable(isVolumeMissing);
            response.setIsWeightEditable(isWeightMissing);
        } else {
            boolean isVolumeMissing = Boolean.FALSE;
            for(Packing packing : packings) {
                if(Objects.isNull(packing.getVolume())) {
                    isVolumeMissing = Boolean.TRUE;
                    break;
                }
            }
            response.setIsVolumeEditable(isVolumeMissing);
        }
    }

    @Override
    public CargoChargeableResponse calculateChargeable(CargoChargeableRequest request) throws RunnerException {
        VolumeWeightChargeable vwOb = consolidationService.calculateVolumeWeight(
                request.getTransportMode(),
                request.getWeightUnit(),
                request.getVolumeUnit(),
                request.getWeight(),
                request.getVolume()
        );

        BigDecimal chargeable = vwOb.getChargeable();
        if (Constants.TRANSPORT_MODE_AIR.equalsIgnoreCase(request.getTransportMode())) {
            chargeable = BigDecimal.valueOf(roundOffAirShipment(chargeable.doubleValue()));
        }

        CargoChargeableResponse response = new CargoChargeableResponse();
        response.setWeight(request.getWeight());
        response.setWeightUnit(request.getWeightUnit());
        response.setChargeable(chargeable);
        response.setChargeableUnit(vwOb.getChargeableUnit());
        response.setVolumetricWeight(vwOb.getVolumeWeight());
        response.setVolumetricWeightUnit(vwOb.getVolumeWeightUnit());
        return response;
    }

    private double roundOffAirShipment(double charge) {
        return (charge - 0.50 <= Math.floor(charge) && charge != Math.floor(charge)) ?
                Math.floor(charge) + 0.5 : Math.ceil(charge);
    }
}