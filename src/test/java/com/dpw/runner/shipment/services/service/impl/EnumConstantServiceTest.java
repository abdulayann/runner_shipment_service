package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class EnumConstantServiceTest {

    @InjectMocks
    private EnumConstantService enumConstantService;

    @Test
    void enumList() {
        ResponseEntity<IRunnerResponse> httpResponse = enumConstantService.list();
        assertEquals(HttpStatus.OK, httpResponse.getStatusCode());
    }
}
