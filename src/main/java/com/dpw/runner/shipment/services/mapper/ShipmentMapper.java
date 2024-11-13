package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.response.ShipmentListResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper(
    uses = { CarrierDetailsMappers.class, PartiesResponseMapper.class, PickupDeliveryDetailsListResponseMapper.class, AdditionalDetailsListResponseMapper.class }
)
public interface ShipmentMapper {

  ShipmentMapper INSTANCE = Mappers.getMapper(ShipmentMapper.class);
  ShipmentListResponse toShipmentListResponse(ShipmentDetails shipmentDetails);
}
