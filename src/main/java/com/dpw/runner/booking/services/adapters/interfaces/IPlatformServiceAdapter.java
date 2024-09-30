package com.dpw.runner.booking.services.adapters.interfaces;

import com.dpw.runner.booking.services.commons.requests.CommonRequestModel;
import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import com.dpw.runner.booking.services.exception.exceptions.RunnerException;
import org.springframework.http.ResponseEntity;

public interface IPlatformServiceAdapter {
    ResponseEntity<IRunnerResponse> createAtPlatform(CommonRequestModel requestModel) throws RunnerException;

    ResponseEntity<IRunnerResponse> updateAtPlaform(CommonRequestModel requestModel) throws RunnerException;

}
