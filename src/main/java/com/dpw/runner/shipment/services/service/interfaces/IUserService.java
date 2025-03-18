package com.dpw.runner.shipment.services.service.interfaces;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import org.springframework.stereotype.Service;

@Service
public interface IUserService {
    UsersDto getUserByToken(String token);
}
