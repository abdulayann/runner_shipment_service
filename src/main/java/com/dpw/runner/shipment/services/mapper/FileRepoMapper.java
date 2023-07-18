package com.dpw.runner.shipment.services.mapper;

import com.dpw.runner.shipment.services.dto.request.FileRepoRequest;
import com.dpw.runner.shipment.services.dto.response.FileRepoResponse;
import com.dpw.runner.shipment.services.entity.FileRepo;
import org.mapstruct.*;

@Mapper(
        uses = JsonNullableMapper.class,
        nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE,
        componentModel = MappingConstants.ComponentModel.SPRING,
        unmappedTargetPolicy = ReportingPolicy.IGNORE
)
public interface FileRepoMapper {

    FileRepo map(FileRepoRequest req);
    FileRepoResponse map(FileRepo entity);
    FileRepoRequest getRequest(FileRepo entity);

    @InheritConfiguration
    void update(FileRepoRequest update, @MappingTarget FileRepo destination);
}
