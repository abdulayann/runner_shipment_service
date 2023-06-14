package com.dpw.runner.shipment.services.service;

import com.dpw.runner.shipment.services.dto.UsersDto;
import org.springframework.stereotype.Service;

@Service
public interface IUserService {
    UsersDto getUserByUserName(String userName);
}
