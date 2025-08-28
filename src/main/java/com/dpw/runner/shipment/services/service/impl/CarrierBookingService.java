package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingListRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class CarrierBookingService implements ICarrierBookingService {

    private final ICarrierBookingDao carrierBookingDao;
    private final JsonHelper jsonHelper;

    @Autowired
    public CarrierBookingService(ICarrierBookingDao carrierBookingDao, JsonHelper jsonHelper) {
        this.carrierBookingDao = carrierBookingDao;
        this.jsonHelper = jsonHelper;
    }

    @Override
    public CarrierBookingResponse create(CarrierBookingRequest request) {
        log.info("CarrierBookingService.create() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        CarrierBooking  savedEntity = carrierBookingDao.create(carrierBookingEntity);
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);
        log.info("CarrierBookingService.create() successful with RequestId: {} and response: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    @Override
    public CarrierBookingResponse findById(Long id) {
        log.info("CarrierBookingService.getById() called with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
        CarrierBooking carrierBooking = carrierBookingDao.findById(id).orElseThrow(() -> new ValidationException("Invalid id : " + id));
        CarrierBookingResponse carrierBookingResponse = jsonHelper.convertValue(carrierBooking, CarrierBookingResponse.class);
        log.info("CarrierBookingService.getById() successful with RequestId: {} and response: {}",
                LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(carrierBookingResponse));
        return carrierBookingResponse;
    }

    @Override
    public CarrierBookingListResponse list(CarrierBookingListRequest request) {
        log.info("CarrierBookingService.list() called with RequestId: {} and payload: {}",
                LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        CarrierBookingListResponse response = carrierBookingDao.list(request);
        log.info("CarrierBookingService.list() successful with RequestId: {}. Records: {}, Pages: {}",
                LoggerHelper.getRequestIdFromMDC(), response.getNumberOfRecords(), response.getTotalPages());
        return response;
    }

    @Override
    public CarrierBookingResponse update(Long id, CarrierBookingRequest request) {
        log.info("CarrierBookingService.update() called with RequestId: {}, id: {} and payload: {}",
                LoggerHelper.getRequestIdFromMDC(), id, jsonHelper.convertToJson(request));
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        CarrierBooking  savedEntity = carrierBookingDao.update(id, carrierBookingEntity);
        CarrierBookingResponse response = jsonHelper.convertValue(savedEntity, CarrierBookingResponse.class);
        log.info("CarrierBookingService.update() successful with RequestId: {} and response: {}",
                LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(response));
        return response;
    }

    @Override
    public void delete(Long id) {
        log.info("CarrierBookingService.delete() called with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
        carrierBookingDao.delete(id);
        log.info("CarrierBookingService.delete() successful with RequestId: {} and id: {}",
                LoggerHelper.getRequestIdFromMDC(), id);
    }
}

