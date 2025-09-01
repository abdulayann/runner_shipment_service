package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.request.carrierbooking.ShippingInstructionRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ShippingInstructionResponse;
import com.dpw.runner.shipment.services.entity.ShippingInstruction;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShippingInstructionRepository;
import com.dpw.runner.shipment.services.service.interfaces.IShippingInstructionsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class IShippingInstructionsServiceImpl implements IShippingInstructionsService {

    @Autowired
    private IShippingInstructionRepository repository;

    @Autowired
    private JsonHelper jsonHelper;


    public ShippingInstructionResponse createShippingInstruction(ShippingInstructionRequest info) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(info, ShippingInstruction.class);
        ShippingInstruction savedInfo = repository.save(shippingInstruction);
        return jsonHelper.convertValue(savedInfo, ShippingInstructionResponse.class);
    }

    public ShippingInstructionResponse getShippingInstructionsById(Long id) {
        Optional<ShippingInstruction> shippingInstruction = repository.findById(id);
        return jsonHelper.convertValue(shippingInstruction, ShippingInstructionResponse.class);
    }

    public ShippingInstructionResponse updateShippingInstructions(ShippingInstructionRequest updatedInfo) {
        ShippingInstruction shippingInstruction = jsonHelper.convertValue(updatedInfo, ShippingInstruction.class);
        ShippingInstruction information = repository.save(shippingInstruction);
        return jsonHelper.convertValue(information, ShippingInstructionResponse.class);
    }

    public void deleteShippingInstructions(Long id) {
        repository.deleteById(id);
    }
}
