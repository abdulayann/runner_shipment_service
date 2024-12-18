package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.ArrivalDepartureDetailsResponse;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import java.util.List;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface ArrivalDepartureDetailsMapper {

  ArrivalDepartureDetailsMapper INSTANCE = Mappers.getMapper(ArrivalDepartureDetailsMapper.class);

  ArrivalDepartureDetailsResponse toArrivalDepartureDetailsResponse(
      ArrivalDepartureDetails arrivalDepartureDetails);

  List<ArrivalDepartureDetailsResponse> toArrivalDepartureDetailsResponses(
      List<ArrivalDepartureDetails> arrivalDepartureDetails);
}
