package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import org.springframework.http.ResponseEntity;

public interface ICarrierBookingService {

    /**
     * Create a new Carrier Booking.
     *
     * @param request CarrierBookingRequest
     * @return CarrierBookingResponse
     */
    CarrierBookingResponse create(CarrierBookingRequest request);

    /**
     * Retrieve a Carrier Booking by its ID.
     *
     * @param id booking id
     * @return CarrierBookingResponse
     */
    CarrierBookingResponse findById(Long id);

    ResponseEntity<IRunnerResponse> list(CommonRequestModel listCommonRequest, boolean getMasterData);

    /**
     * Update an existing Carrier Booking.
     *
     * @param id booking id
     * @param request CarrierBookingRequest
     * @return CarrierBookingResponse
     */
    CarrierBookingResponse update(Long id, CarrierBookingRequest request);

    /**
     * Delete a Carrier Booking by its ID.
     *
     * @param id booking id
     */
    void delete(Long id);
}

