package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.commons.dto.request.UsersDto;
import org.springframework.stereotype.Service;

@Service
public interface IUserService {
    UsersDto getUserByToken(String key, String token);
}
