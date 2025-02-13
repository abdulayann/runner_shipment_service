package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper
public interface AchievedQuantitiesMapper {

    AchievedQuantitiesMapper INSTANCE = Mappers.getMapper(AchievedQuantitiesMapper.class);

    AchievedQuantitiesResponse toAchievedQuantitiesResponse(AchievedQuantities achievedQuantities);

    List<AchievedQuantitiesResponse> toAchievedQuantitiesResponses(
            List<AchievedQuantities> achievedQuantities);
}
