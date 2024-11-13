package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface CarrierDetailsMappers {

  CarrierDetailsMapper INSTANCE = Mappers.getMapper(CarrierDetailsMapper.class);

  CarrierDetailResponse toCarrierDetailsResponse(CarrierDetails carrierDetails);
}
