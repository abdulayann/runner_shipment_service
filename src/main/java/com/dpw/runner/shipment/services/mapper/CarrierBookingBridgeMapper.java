package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingBridgeRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CarrierBookingResponse;
import org.mapstruct.Builder;
import org.mapstruct.Mapper;
import org.mapstruct.MappingConstants;

@Mapper(
        componentModel = MappingConstants.ComponentModel.SPRING,
        builder = @Builder(disableBuilder = true)
)
public interface CarrierBookingBridgeMapper {

    CarrierBookingBridgeRequest mapToBridgeRequest(CarrierBookingResponse response);
}
