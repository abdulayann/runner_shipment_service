package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.SyncBookingToService;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import com.dpw.runner.shipment.services.kafka.dto.inttra.InttraCarrierBookingEventDto;
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
    CarrierBookingResponse retrieveById(Long id);

    ResponseEntity<IRunnerResponse> list(CommonRequestModel listCommonRequest, boolean getMasterData);

    /**
     * Update an existing Carrier Booking.
     *
     *
     * @param request CarrierBookingRequest
     * @return CarrierBookingResponse
     */
    CarrierBookingResponse update(CarrierBookingRequest request);

    /**
     * Delete a Carrier Booking by its ID.
     *
     * @param id booking id
     */
    void delete(Long id);

    void syncCarrierBookingToService(SyncBookingToService syncBookingToService);

    void updateCarrierDataToBooking(InttraCarrierBookingEventDto inttraCarrierBookingEventDto);

    ResponseEntity<IRunnerResponse> getAllMasterData(Long shipmentId);
}

