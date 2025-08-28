package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingListRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingListResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;

import java.util.List;

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
    CarrierBookingResponse getById(Long id);

    /**
     * Retrieve a list of Carrier Bookings with pagination/filter criteria.
     *
     * @param request CommonV1ListRequest
     * @return RunnerListResponse of CarrierBookingResponse
     */
    CarrierBookingListResponse list(CarrierBookingListRequest request);

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

