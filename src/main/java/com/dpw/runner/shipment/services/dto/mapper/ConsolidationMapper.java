package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.ConsolidationListResponse;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import java.util.List;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper(
    uses = {CarrierDetailsMapper.class, PartiesResponseMapper.class, AchievedQuantitiesMapper.class,
        AllocationsMapper.class, ArrivalDepartureDetailsMapper.class}
)
public interface ConsolidationMapper {

  ConsolidationMapper INSTANCE = Mappers.getMapper(ConsolidationMapper.class);

  ConsolidationListResponse toConsolidationListResponse(ConsolidationDetails consolidationDetails);

  List<ConsolidationListResponse> toConsolidationListResponses(
      List<ConsolidationDetails> consolidationDetails);
}