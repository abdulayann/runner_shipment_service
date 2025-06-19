package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsContainersRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsContainersResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.lang.reflect.InvocationTargetException;


public interface ITransportInstructionLegsContainersService {
    TransportInstructionLegsContainersResponse create(TransportInstructionLegsContainersRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsContainersResponse update(TransportInstructionLegsContainersRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsContainersListResponse list(ListCommonRequest request);

    TransportInstructionLegsContainersResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsContainersResponse retrieveById(Long id);
}
