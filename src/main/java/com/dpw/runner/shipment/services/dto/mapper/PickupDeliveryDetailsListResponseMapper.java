package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsListResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import java.util.List;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface PickupDeliveryDetailsListResponseMapper {

  PickupDeliveryDetailsListResponseMapper INSTANCE = Mappers.getMapper(
      PickupDeliveryDetailsListResponseMapper.class);

  // Mapping method from PartiesResponse to PartiesResponseDTO
  PickupDeliveryDetailsListResponse toPickupDeliveryDetailsListResponse(
      PickupDeliveryDetails parties);
  List<PickupDeliveryDetailsListResponse> toPickupDeliveryDetailsListResponses(
      List<PickupDeliveryDetails> parties);

}

