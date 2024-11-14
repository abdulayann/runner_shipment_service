package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface PartiesResponseMapper {

  PartiesResponseMapper INSTANCE = Mappers.getMapper(PartiesResponseMapper.class);

  // Mapping method from PartiesResponse to PartiesResponseDTO
  PartiesResponse toPartiesResponse(Parties parties);
}
