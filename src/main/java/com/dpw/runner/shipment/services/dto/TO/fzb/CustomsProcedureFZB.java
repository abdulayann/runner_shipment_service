package com.dpw.runner.shipment.services.dto.TO.fzb;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.Pattern;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class CustomsProcedureFZB {

    @Pattern(regexp = "[A-Za-z1-9]{1,2}", message = "Goods status code must contain only uppercase and lowercase letters or digits 1-9, with a maximum length of 2 characters")
    private String goodsStatusCode;
}
