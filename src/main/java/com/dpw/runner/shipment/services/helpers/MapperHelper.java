package com.dpw.runner.shipment.services.helpers;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import org.springframework.stereotype.Component;

@Getter
@Component
public class MapperHelper {

    private final ObjectMapper objectMapper = new ObjectMapper();
}
