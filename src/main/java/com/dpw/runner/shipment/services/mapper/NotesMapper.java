package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.NotesRequest;
import com.dpw.runner.shipment.services.dto.response.NotesResponse;
import com.dpw.runner.shipment.services.entity.Notes;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface NotesMapper {

    Notes map(NotesRequest req);
    NotesResponse map(Notes entity);
    NotesRequest getRequest(Notes entity);

    @InheritConfiguration
    void update(NotesRequest update, @MappingTarget Notes destination);
}
