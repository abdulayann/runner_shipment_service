package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IMDMServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.VolumeWeightChargeable;
import com.dpw.runner.shipment.services.dto.request.CargoChargeableRequest;
import com.dpw.runner.shipment.services.dto.request.CargoDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.CargoChargeableResponse;
import com.dpw.runner.shipment.services.dto.response.CargoDetailsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICargoService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.*;

@Service
@Slf4j
public class CargoService implements ICargoService {

    private final IMDMServiceAdapter mdmServiceAdapter;
    private final ICustomerBookingDao customerBookingDao;
    private final IShipmentDao shipmentDao;
    private final IConsolidationDetailsDao consolidationDetailsDao;
    private final JsonHelper jsonHelper;
    private final IConsolidationService consolidationService;

    @Autowired
    public CargoService(IMDMServiceAdapter mdmServiceAdapter,
                        ICustomerBookingDao customerBookingDao,
                        IShipmentDao shipmentDao,
                        IConsolidationDetailsDao consolidationDetailsDao,
                        JsonHelper jsonHelper,
                        IConsolidationService consolidationService) {
        this.mdmServiceAdapter = mdmServiceAdapter;
        this.customerBookingDao = customerBookingDao;
        this.shipmentDao = shipmentDao;
        this.consolidationDetailsDao = consolidationDetailsDao;
        this.jsonHelper = jsonHelper;
        this.consolidationService = consolidationService;
    }

    @Override
    public CargoDetailsResponse getCargoDetails(CargoDetailsRequest request) throws RunnerException {
        CargoDetailsResponse response = new CargoDetailsResponse();
        String entityType = request.getEntityType();
        Long entityId = Long.valueOf(request.getEntityId());
        Optional<CustomerBooking> optionalCustomerBooking = customerBookingDao.findById(entityId);
        if(optionalCustomerBooking.isEmpty()) {
            throw new ValidationException("Booking not found with id "+entityId);
        }
        CustomerBooking customerBooking = optionalCustomerBooking.get();
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
        return response;
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