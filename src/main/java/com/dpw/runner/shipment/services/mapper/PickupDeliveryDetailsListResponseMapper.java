package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsListResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface PickupDeliveryDetailsListResponseMapper {

  PickupDeliveryDetailsListResponseMapper INSTANCE = Mappers.getMapper(
      PickupDeliveryDetailsListResponseMapper.class);

  // Mapping method from PartiesResponse to PartiesResponseDTO
  PickupDeliveryDetailsListResponse toPickupDeliveryDetailsListResponse(
      PickupDeliveryDetails parties);

  PickupDeliveryDetailsListResponse pickupDeliveryDetails(
      PickupDeliveryDetailsListResponse parties);
}
