package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import com.dpw.runner.shipment.services.entity.Allocations;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper
public interface AllocationsMapper {

    AllocationsMapper INSTANCE = Mappers.getMapper(AllocationsMapper.class);

    AllocationsResponse toAllocationsResponse(Allocations allocations);

    List<AllocationsResponse> toAllocationsResponses(
            List<Allocations> allocations);
}
