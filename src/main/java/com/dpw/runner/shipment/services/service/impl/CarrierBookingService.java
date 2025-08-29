package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CarrierBookingConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.ICarrierBookingService;
import com.dpw.runner.shipment.services.utils.v3.CarrierBookingValidationUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Slf4j
@Service
public class CarrierBookingService implements ICarrierBookingService {

    private final ICarrierBookingDao carrierBookingDao;
    private final JsonHelper jsonHelper;
    private final CarrierBookingValidationUtil carrierBookingValidationUtil;

    @Autowired
    public CarrierBookingService(ICarrierBookingDao carrierBookingDao, JsonHelper jsonHelper, CarrierBookingValidationUtil carrierBookingValidationUtil) {
        this.carrierBookingDao = carrierBookingDao;
        this.jsonHelper = jsonHelper;
        this.carrierBookingValidationUtil = carrierBookingValidationUtil;
    }

    @Override
    public CarrierBookingResponse create(CarrierBookingRequest request) {
        log.info("CarrierBookingService.create() called with RequestId: {} and payload: {}", LoggerHelper.getRequestIdFromMDC(), jsonHelper.convertToJson(request));
        carrierBookingValidationUtil.validateRequest(request);
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        CarrierBooking savedEntity = carrierBookingDao.create(carrierBookingEntity);
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
    public CarrierBookingListResponse list(CommonRequestModel commonRequestModel, boolean getMasterData) {
        ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
        Pair<Specification<CarrierBooking>, Pageable> tuple = fetchData(request, CarrierBooking.class, CarrierBookingConstants.tableNames);
        Page<CarrierBooking> carrierBookingPage = carrierBookingDao.findAll(tuple.getLeft(), tuple.getRight());
        List<CarrierBookingResponse> carrierBookingResponseList = convertEntityListToDtoList(carrierBookingPage.getContent(), getMasterData);

        CarrierBookingListResponse carrierBookingListResponse = CarrierBookingListResponse.builder()
                .carrierBookingResponseList(carrierBookingResponseList)
                .totalPages(carrierBookingPage.getTotalPages())
                .numberOfRecords(carrierBookingPage.getTotalElements())
                .build();
        log.info("CarrierBookingService.list() successful with RequestId: {}. Records: {}, Pages: {}",
                LoggerHelper.getRequestIdFromMDC(), carrierBookingListResponse.getNumberOfRecords(), carrierBookingListResponse.getTotalPages());
        return carrierBookingListResponse;
    }

    private List<CarrierBookingResponse> convertEntityListToDtoList(List<CarrierBooking> carrierBookingList, boolean getMasterData) {
        return new ArrayList<>();
    }

    @Override
    public CarrierBookingResponse update(Long id, CarrierBookingRequest request) {
        log.info("CarrierBookingService.update() called with RequestId: {}, id: {} and payload: {}",
                LoggerHelper.getRequestIdFromMDC(), id, jsonHelper.convertToJson(request));
        CarrierBooking carrierBookingEntity = jsonHelper.convertValue(request, CarrierBooking.class);
        CarrierBooking savedEntity = carrierBookingDao.update(id, carrierBookingEntity);
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

