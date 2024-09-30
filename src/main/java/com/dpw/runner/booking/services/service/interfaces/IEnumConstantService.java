package com.dpw.runner.booking.services.service.interfaces;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

public interface IEnumConstantService {
    ResponseEntity<IRunnerResponse> list();
}
