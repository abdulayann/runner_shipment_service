package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
public class SyncEntityConversionService {

    @Autowired
    private ModelMapper modelMapper;

    public List<PackingRequestV2> packingsV2ToV1(List<Packing> packingList, List<Containers> containers) {
        Map<Long, String> map = new HashMap<>();
        if(containers != null)
            map = containers.stream().filter(container -> container.getContainerNumber() != null).collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        if(packingList != null) {
            Map<Long, String> finalMap = map;
            List<PackingRequestV2> res = packingList.stream()
                    .map(item -> {
                        PackingRequestV2 p;
                        p = modelMapper.map(item, PackingRequestV2.class);
                        p.setOriginName(item.getOrigin());
                        p.setOrigin(null);
                        if(item.getContainerId() != null && finalMap.containsKey(item.getContainerId()))
                            p.setContainerNumber(finalMap.get(item.getContainerId()));
                        return p;
                    })
                    .collect(Collectors.toList());
            return res;
        }
        return new ArrayList<>();
    }

    public List<ContainerRequestV2> containersV2ToV1(List<Containers> containersList) {
        if(containersList != null) {
            List<ContainerRequestV2> res = containersList.stream().map(
                    i -> {
                        var containerRequestV2 = modelMapper.map(i, ContainerRequestV2.class);
                        containerRequestV2.setIsHazardous(i.getHazardous());
                        containerRequestV2.setDgClassString(i.getDgClass());
                        containerRequestV2.setMarksnNums(i.getMarksNums());
                        containerRequestV2.setContainerStuffingLocationName(i.getContainerStuffingLocation());
                        return containerRequestV2;
                    }
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public List<Containers> containersV1ToV2(List<ContainerRequestV2> containersList) {
        if(containersList != null) {
            List<Containers> res = containersList.stream().map(
                    this::containerV1ToV2
            ).toList();
            return res;
        }
        return new ArrayList<>();
    }

    public Containers containerV1ToV2(ContainerRequestV2 containerRequestV2) {
        var containers = modelMapper.map(containerRequestV2, Containers.class);
        containers.setHazardous(containerRequestV2.getIsHazardous());
        containers.setDgClass(containerRequestV2.getDgClassString());
        containers.setMarksNums(containerRequestV2.getMarksnNums());
        containers.setContainerStuffingLocation(containerRequestV2.getContainerStuffingLocationName());
        return containers;
    }

}
