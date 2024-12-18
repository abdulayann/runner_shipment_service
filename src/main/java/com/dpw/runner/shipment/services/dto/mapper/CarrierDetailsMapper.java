package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import java.util.List;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface CarrierDetailsMapper {

  CarrierDetailsMapper INSTANCE = Mappers.getMapper(CarrierDetailsMapper.class);

  CarrierDetailResponse toCarrierDetailsResponse(CarrierDetails carrierDetails);
  List<CarrierDetailResponse> toCarrierDetailsResponses(List<CarrierDetails> carrierDetails);
}
