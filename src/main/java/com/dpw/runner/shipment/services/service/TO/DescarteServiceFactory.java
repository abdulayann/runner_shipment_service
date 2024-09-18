package com.dpw.runner.shipment.services.service.TO;

import com.dpw.runner.shipment.services.commons.EAWBConstants;
import com.dpw.runner.shipment.services.service.TO.impl.FWBService;
import com.dpw.runner.shipment.services.service.TO.impl.FZBService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
public class DescarteServiceFactory {

    @Autowired
    FWBService fwbService;

    @Autowired
    FZBService fzbService;

    public IDescartes getMessageService(String type) {
        if (Objects.equals(type, EAWBConstants.MAWB) || Objects.equals(type, EAWBConstants.DMAWB))
            return fwbService;
        if (Objects.equals(type, EAWBConstants.HAWB))
            return fzbService;
        throw new RuntimeException("Invalid MessageType:--" + type);
    }
}
