package com.dpw.runner.shipment.services.commons.responses;


import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * /**
 *  * Generic List response.
 *
 */
@SuppressWarnings("rawtypes")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RunnerPartialListResponse implements IRunnerResponse {
   Object data;
}


