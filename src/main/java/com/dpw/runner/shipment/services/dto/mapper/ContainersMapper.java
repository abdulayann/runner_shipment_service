package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface ContainersMapper {

  ContainersMapper INSTANCE = Mappers.getMapper(ContainersMapper.class);

  ContainerResponse toContainerResponse(Containers containers);
  Containers toContainers(Containers containers);
}
