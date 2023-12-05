package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
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
                        if(item.getContainerId() != null && finalMap.containsKey(item.getContainerId()))
                            p.setContainerNumber(finalMap.get(item.getContainerId()));
                        return p;
                    })
                    .collect(Collectors.toList());
            return res;
        }
        return new ArrayList<>();
    }

}
