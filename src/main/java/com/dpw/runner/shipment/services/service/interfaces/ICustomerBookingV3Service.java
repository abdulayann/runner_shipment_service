package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CreditLimitRequest;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingV3Request;
import com.dpw.runner.shipment.services.dto.request.platformBooking.PlatformToRunnerCustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.response.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;


import javax.validation.Valid;
import java.util.Map;

public interface ICustomerBookingV3Service {
    CustomerBookingV3Response create(CustomerBookingV3Request customerBookingV3Request) throws RunnerException;
    CustomerBookingV3Response update(CustomerBookingV3Request customerBookingV3Request) throws RunnerException;
    CustomerBookingV3DeleteResponse delete(@Valid Long id) throws RunnerException;
    CustomerBookingV3ListResponse list(@Valid ListCommonRequest listCommonRequest) throws RunnerException;
    CustomerBookingV3Response retrieveById(CommonGetRequest request) throws RunnerException;
    CustomerBookingV3Response cloneBooking(Long id) throws RunnerException;
    CustomerBookingV3Response retrieveByOrderId(String orderId) throws RunnerException;
    PlatformToRunnerCustomerBookingResponse platformCreateBooking(@Valid PlatformToRunnerCustomerBookingRequest request) throws RunnerException;
    CheckCreditLimitResponse checkCreditLimitFromFusion(CreditLimitRequest creditLimitRequest) throws RunnerException;
    V1ShipmentCreationResponse retryForBilling(CommonGetRequest commonGetRequest) throws RunnerException;
    Map<String, Object> getAllMasterData(Long bookingId);
    CustomerBookingV3Response findByBookingNumber(String bookingNumber);
    CustomerBookingV3Response getDefaultBooking();
}
