package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.syncing.Entity.PackingRequestV2;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class SyncEntityConversionService {

    @Autowired
    private ModelMapper modelMapper;

    public List<PackingRequestV2> packingsV2ToV1(List<Packing> packingList, List<Containers> containers) {
        Map<Long, String> map = containers.stream().collect(Collectors.toMap(Containers::getId, Containers::getContainerNumber));
        List<PackingRequestV2> res = packingList.stream()
                .map(item -> {
                    PackingRequestV2 p;
                    p = modelMapper.map(item, PackingRequestV2.class);
                    p.setOriginName(item.getOrigin());
                    if(item.getContainerId() != null && map.containsKey(item.getContainerId()))
                        p.setContainerNumber(map.get(item.getContainerId()));
                    return p;
                })
                .collect(Collectors.toList());
        return res;
    }

}
