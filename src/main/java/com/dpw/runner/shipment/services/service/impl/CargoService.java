package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CargoChargeableRequest;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoChargeableResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import com.dpw.runner.shipment.services.service.interfaces.ICustomerBookingV3Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;

@Service
@Slf4j
public class CargoService implements ICargoService {

    private final ICustomerBookingDao customerBookingDao;
    private final ConsolidationV3Service consolidationService;
    private final ICustomerBookingV3Service customerBookingV3Service;
    private final CommonUtils commonUtils;

    @Autowired
    public CargoService(ICustomerBookingDao customerBookingDao,
                        ConsolidationV3Service consolidationService,
                        ICustomerBookingV3Service customerBookingV3Service,
                        CommonUtils commonUtils) {
        this.customerBookingDao = customerBookingDao;
        this.consolidationService = consolidationService;
        this.customerBookingV3Service = customerBookingV3Service;
        this.commonUtils = commonUtils;
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
        response.setIsDifferenceInPackages(calculatePackageDifference(containers, packings) ? Boolean.TRUE : Boolean.FALSE);
        response.setIsDifferenceInCargoWeight(calculateCargoWeightDifference(containers, packings));
        return response;
    }

    private boolean calculatePackageDifference(List<Containers> containersList, List<Packing> packingList) {
        if(containersList.isEmpty() || packingList.isEmpty()) {
            return false;
        }
        Long totalContainerPackages = customerBookingV3Service.getTotalContainerPackages(containersList);
        Long totalPackingSectionPackages = 0L;

        for(Packing pack: packingList) {
            totalPackingSectionPackages += Long.parseLong(pack.getPacks());
        }
        return Math.abs(totalContainerPackages - totalPackingSectionPackages) > 0L;
    }

    private boolean calculateCargoWeightDifference(List<Containers> containersList, List<Packing> packingList) throws RunnerException {
        if(containersList.isEmpty() || packingList.isEmpty()) {
            return false;
        }
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        String weightUnit = consolidationService.determineWeightChargeableUnit(shipmentSettingsDetails);
        BigDecimal totalContainerCargoWeight = customerBookingV3Service.getTotalCargoWeight(containersList, weightUnit);
        BigDecimal totalPackingSectionCargoWeight = BigDecimal.ZERO;

        for(Packing pack: packingList) {
            if(pack.getWeight() != null) {
                totalPackingSectionCargoWeight = totalPackingSectionCargoWeight.add(pack.getWeight());
            }
        }

        return totalContainerCargoWeight.subtract(totalPackingSectionCargoWeight).abs().compareTo(BigDecimal.ZERO) > 0;
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