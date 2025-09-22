package com.dpw.runner.shipment.services.dto.mapper;

import com.dpw.runner.shipment.services.dto.request.ContainerV3Request;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper
public interface ContainersMapper {

  ContainersMapper INSTANCE = Mappers.getMapper(ContainersMapper.class);

  ContainerResponse toContainerResponse(Containers containers);
  Containers toContainers(Containers containers);
  List<ContainerV3Request> toContainerV3RequestList(List<Containers> containers);
}
