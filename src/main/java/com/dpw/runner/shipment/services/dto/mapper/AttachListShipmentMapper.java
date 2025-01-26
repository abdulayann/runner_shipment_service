package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.AttachListShipmentResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(
        uses = { CarrierDetailsMapper.class, PartiesResponseMapper.class, PickupDeliveryDetailsListResponseMapper.class, AdditionalDetailsListResponseMapper.class }
)
public interface AttachListShipmentMapper {
    AttachListShipmentMapper INSTANCE = Mappers.getMapper(AttachListShipmentMapper.class);
    AttachListShipmentResponse toAttachListShipmentResponse(ShipmentDetails shipmentDetails);
    List<AttachListShipmentResponse> toAttachListShipmentResponse(List<ShipmentDetails> shipmentDetails);
}
