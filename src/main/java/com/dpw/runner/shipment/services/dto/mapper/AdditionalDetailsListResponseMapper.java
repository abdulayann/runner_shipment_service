package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.AdditionalDetailsListResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import java.util.List;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

@Mapper(uses = {PartiesResponseMapper.class})
public interface AdditionalDetailsListResponseMapper {

  AdditionalDetailsListResponseMapper INSTANCE = Mappers.getMapper(
      AdditionalDetailsListResponseMapper.class);

   @Mapping(target = "screeningStatus", source = "additionalDetails.screeningStatus")
  AdditionalDetailsListResponse toAdditionalDetailsListResponse(
      AdditionalDetails additionalDetails);
  List<AdditionalDetailsListResponse> toAdditionalDetailsListResponses(
      List<AdditionalDetails> additionalDetails);
  default String map(List<String> value) {
    return value == null ? null : value.toString();
  }
}