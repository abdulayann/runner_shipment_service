package com.dpw.runner.shipment.services.service.interfaces;


import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dto.v3.request.TransportInstructionLegsRequest;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsListResponse;
import com.dpw.runner.shipment.services.dto.v3.response.TransportInstructionLegsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.fasterxml.jackson.core.JsonProcessingException;

import java.lang.reflect.InvocationTargetException;


public interface ITransportInstructionLegsService {
    TransportInstructionLegsResponse create(TransportInstructionLegsRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsResponse update(TransportInstructionLegsRequest request) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsListResponse list(ListCommonRequest request);

    TransportInstructionLegsResponse delete(Long id) throws RunnerException, NoSuchFieldException, JsonProcessingException, InvocationTargetException, IllegalAccessException, NoSuchMethodException;

    TransportInstructionLegsResponse retrieveById(Long id);
}
